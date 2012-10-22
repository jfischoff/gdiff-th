{-#  LANGUAGE TemplateHaskell, QuasiQuotes, TupleSections, 
    RecordWildCards, DeriveDataTypeable #-}
module Data.Generic.Diff.TH.Internal where
import Data.Generic.Diff.TH.Types
import Language.Haskell.TH
import qualified Language.Haskell.TH as TH
import Data.Generic.Diff 
import Control.Monad
import Data.Generics.Uniplate.Data
import Data.Generic.Diff.TH.Specialize
import Data.Generic.Diff.TH.Conversion
import Data.Maybe(fromMaybe)
import Data.Word
import Data.Int
--TODO
--I should check that the root type is monomorphic

--Questions
--performance
--Can I use this to perform a patch that 
--Version control stuff?
--Assuming the formatting is fine

defaultPrimitives :: [(Name, TH.Exp)]
defaultPrimitives = map (, VarE 'show) defaultNames

defaultNames :: [Name]
defaultNames = [
    ''Int     ,
    ''Char    ,
    ''String  ,
    ''Float   ,
    ''Double  ,
    ''Int8    ,
    ''Int16   ,
    ''Int32   ,
    ''Int64   ,
    ''Word    ,
    ''Word8   ,
    ''Word16  ,
    ''Word32  ,
    ''Word64  ,
    ''Integer  ]


toConE :: FamCon -> TH.ExpQ      
toConE (FamCon {..}) = case _famConHardness of 
             Concrete -> conE 'Concr `appE` conE _famConName
             Abstract -> conE 'Abstr `appE` conE _famConName

famInstance :: [(Name, TH.Exp)] -> Fam -> Q Dec
famInstance prims (Fam {..}) = do
    let constrs  = universeBi _famTypes
        
        --utiliy function
        funcFromCons :: (Name, FamCon -> ClauseQ, [ClauseQ]) -> DecQ
        funcFromCons (n, f, extra) = funD n $ map f constrs ++ extra

        -- f _ _ = Nothing
        defaultClause = clause [wildP, wildP] (normalB [e| Nothing |]) []

        decs = map funcFromCons [
                ('decEq , decClause   , [defaultClause]),
                ('apply , applyClause , []             ),
                ('fields, fieldClause , [defaultClause]),
                ('string, stringClause prims, []             )
               ]

        instanceType  = conT ''Family `appT` conT _famName

    instanceD (return []) instanceType decs

decClause :: FamCon -> ClauseQ
decClause (FamCon {..}) = case _famConHardness of
    Concrete -> clause [conP _famConName [], conP _famConName []] 
                            (normalB [e| Just (Refl, Refl) |]) [] 
    Abstract -> do
         x <- newName "x"
         y <- newName "y"
         clause [conP _famConName [varP x], 
                 conP _famConName [varP y]] (normalB
                 [e| if $(varE x) == $(varE y) 
                                then Just (Refl, Refl) 
                                else Nothing |]) []  

stringClause :: [(Name, TH.Exp)] -> FamCon -> ClauseQ
stringClause prims (FamCon {..}) = case _famConHardness of
    Concrete -> clause [conP _famConName []] 
            (normalB . stringE . nameBase $ _famConOriginalName) [] 
    Abstract -> do 
            p <- newName "p"
            let showExp = fromMaybe ( error $ "Logic error." ++ show _famConOriginalName ++ 
                                    " Primitive doesn't have a show TH.Expr") $
                                    lookup _famConOriginalName prims 
            clause [conP _famConName [varP p]] (normalB $ return showExp `appE` varE p) []

fieldClause :: FamCon -> ClauseQ 
fieldClause (FamCon {..}) = case _famConHardness of
    Concrete -> do 
      parameterNames <- replicateM (length _famConTypes) (newName "x") 
      let parameterListP = conP _famConOriginalName $ 
                                map varP parameterNames 
          body = normalB . appE (conE 'Just) $ 
                    foldr (appE . appE (conE 'CCons) . varE) (conE 'CNil) parameterNames 
      clause [conP _famConName [], parameterListP] body []
    Abstract -> clause [conP _famConName [wildP], wildP] (normalB [e| Just CNil |]) []

applyClause :: FamCon -> ClauseQ
applyClause (FamCon {..}) = case _famConHardness of
    Concrete -> do 
           parameterNames <- replicateM (length _famConTypes) 
                               (newName "x") 
           let parameterListP = 
                   foldl (\o n -> conP 'CCons [varP n, o]) (conP 'CNil []) 
                       parameterNames
               body = normalB . foldl (\x y -> appE x $ varE y) 
                                   (conE _famConOriginalName) $ 
                                        reverse parameterNames 
           clause [conP _famConName [], parameterListP] body []
    Abstract -> do
           nx <- newName "x"
           clause [conP _famConName [varP nx], conP 'CNil []] (normalB $ varE nx) [] 

familyTypeInstances :: Fam -> Q [Dec]
familyTypeInstances (Fam {..}) = mapM (typInstance _famName) _famTypes

typInstance :: Name -> FamType -> Q Dec
typInstance familyName (FamType {..}) = do
    --TODO make a helper function to make it clearer what this is doing
    let instanceType = foldl1 appT [conT   ''Data.Generic.Diff.Type ,
                                    conT   familyName                  , 
                                    return _famTypeType              ]

        dec          = funD 'constructors [mainClause]
        mainClause   = clause [] (normalB . listE . map toConE $ _famTypeConstructors) []

    instanceD (return []) instanceType [dec]

mkAllInstances :: [(Name, TH.Exp)] -> Fam -> Q [Dec]
mkAllInstances prims x = liftM2 (:) (famInstance prims x) (familyTypeInstances x)

mkGADTConstructor :: Name -> Name -> TH.Type -> FamCon -> ConQ
mkGADTConstructor a b typ (FamCon {..}) = case _famConHardness of
        Concrete -> forallC [] 
            (sequence [equalP (varT a) (return typ),
                equalP (varT b) (foldr (appT .appT (conT ''Cons) . return) (conT ''Nil) 
                                     _famConTypes)]) 
                            (normalC _famConName [])
        Abstract -> forallC [] (sequence [equalP (varT a) (return typ), 
                            equalP (varT b) (conT ''Nil)]) 
                           (normalC _famConName [return (NotStrict, ConT _famConOriginalName)]) 

mkGADT :: Fam -> Q Dec
mkGADT (Fam {..}) = do
    a <- newName "a"
    b <- newName "b"
    let constrs = 
            concatMap (\(FamType {..}) -> map (mkGADTConstructor a b _famTypeType) _famTypeConstructors) 
                                    _famTypes 

    dataD (return []) _famName [PlainTV a, PlainTV b] constrs []

type ConstructorRenamer = (String -> Name -> TH.Type -> Q Name)

--TODO make this take a renameFunctions: for the family, for the constructors
makeGDiffWith :: String -> ConstructorRenamer -> [(Name, TH.Exp)] -> Name -> Q [Dec]
makeGDiffWith familyPrefix constructorRenamer primitives name = do
    let familyName = mkName $ nameBase name ++ familyPrefix
        prefix     = nameBase name
        
    --check if it is a polymorphic type
    dec <- reify name
    
    when (not $ null [x | VarT x <- universeBi dec]) $ 
        error "type must be monomorphic"


    fam       <- toFam (map fst primitives) (constructorRenamer prefix) 
                    familyName =<< specialize name
    instances <- mkAllInstances primitives fam
    gadt      <- mkGADT fam 

    return $ gadt : instances
    
--TODO make something better as a default
defaultConstructorRenamer :: String -> Name -> TH.Type -> Q Name
defaultConstructorRenamer prefix n typ = return . mkName $ 
        filter (\x -> x /= '[' && x /= ']') $ prefix ++ 
           typToString typ ++ prettifyName n ++ "C"
           
defaultFamSuffix :: String
defaultFamSuffix = "Family"
    
makeGDiff :: Name -> Q [Dec]
makeGDiff = makeGDiffWith defaultFamSuffix defaultConstructorRenamer defaultPrimitives






