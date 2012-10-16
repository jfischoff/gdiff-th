{-#  LANGUAGE TemplateHaskell, QuasiQuotes, RecordWildCards, DeriveDataTypeable #-}
module Data.Generic.Diff.TH where
import Data.Generic.Diff.TH.Types
import Language.Haskell.TH
import Data.List (nub)
import Debug.Trace
import qualified Language.Haskell.TH as TH
import Data.Generic.Diff
import Data.Word
import Data.Int
import Data.Data
import Generics.Pointless.MonadCombinators
import Control.Lens
import Control.Monad
import Data.Generics.Uniplate.Data
import Data.Map (Map, fromList) 
import qualified Data.Map as M
import Control.Applicative 
import Data.Maybe
--import Language.Haskell.TH.Lens
import Control.Arrow
import Language.Haskell.TH.ExpandSyns
import Language.Haskell.TH.Universe
import Language.Haskell.TH.Syntax (NameFlavour)
import Data.Generic.Diff.Specialize
import Debug.Trace

traceIt x = trace (show x) x
--TODO
--I should pass in what the primitives are
--how to display them
--I should pass in the prefix optionally
--and the way things get renamed
--I should check that the root type is monomorphic

--Questions
--Can I use this to perform a patch that 
--Version control stuff?
--Assuming the formatting is fine

   
toConE :: FamCon -> ExpQ      
toConE (FamCon {..}) = case _famConHardness of 
             Concrete -> conE 'Concr `appE` conE _famConName
             Abstract -> conE 'Abstr `appE` conE _famConName

famInstance :: Fam -> Q Dec
famInstance (Fam {..}) = do
    let constructors  = universeBi _famTypes
        
        funcFromCons :: (Name, (FamCon -> ClauseQ), [ClauseQ]) -> DecQ
        funcFromCons (n, f, extra) = funD n $ map f constructors ++ extra
        
        defaultClause = clause [wildP, wildP] (normalB [e| Nothing |]) []
        
        decs = map funcFromCons [
                ('decEq , decClause   , [defaultClause]),
                ('apply , applyClause , []             ),
                ('fields, fieldClause , [defaultClause]),
                ('string, stringClause, []             )
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

stringClause :: FamCon -> ClauseQ
stringClause (FamCon {..}) = case _famConHardness of
    Concrete -> clause [conP _famConName []] 
            (normalB . stringE . nameBase $ _famConOriginalName) [] 
    Abstract -> do 
            p <- newName "p"
            clause [conP _famConName [varP p]] (normalB [e| show $(varE p)|]) []

fieldClause :: FamCon -> ClauseQ 
fieldClause (FamCon {..}) = case _famConHardness of
    Concrete -> do 
      parameterNames <- replicateM (length _famConTypes) (newName "x") 
      let parameterListP = conP _famConOriginalName $ 
                                map varP parameterNames 
          body = normalB . appE (conE 'Just) $ foldr appE (conE 'CNil) $ 
                            map (appE (conE 'CCons) . varE) parameterNames 
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
typInstance famName (FamType {..}) = do
    --TODO make a helper function to make it clearer what this is doing
    let instanceType = foldl1 appT [conT   ''Data.Generic.Diff.Type ,
                                    conT   famName                  , 
                                    return _famTypeType              ]
        
        dec          = funD 'constructors [mainClause]
        mainClause   = clause [] (normalB . listE . map toConE $ _famTypeConstructors) []
            
    instanceD (return []) instanceType [dec]

mkAllInstances :: Fam -> Q [Dec]
mkAllInstances x = liftM2 (:) (famInstance x) (familyTypeInstances x)

mkGADTConstructor :: Name -> Name -> TH.Type -> FamCon -> ConQ
mkGADTConstructor a b typ (FamCon {..}) = do 
    case _famConHardness of
        Concrete -> forallC [] 
            (sequence [equalP (varT a) (return typ),
                equalP (varT b) (foldr appT (conT ''Nil) $ 
                    map (appT (conT ''Data.Generic.Diff.Cons) 
                                    . return) _famConTypes)]) 
                            (normalC _famConName [])
        Abstract -> forallC [] (sequence [equalP (varT a) (return typ), 
                            equalP (varT b) (conT ''Nil)]) 
                           (normalC _famConName [return (NotStrict, ConT _famConOriginalName)]) 

mkGADT :: Fam -> Q Dec
mkGADT (Fam {..}) = do
    a <- newName "a"
    b <- newName "b"
    let constructors = 
            concatMap (\(FamType {..}) -> map (mkGADTConstructor a b _famTypeType) _famTypeConstructors) 
                                    _famTypes 

    dataD (return []) _famName [PlainTV a, PlainTV b] constructors []


isPrimitiveType :: TH.Type -> Bool 
isPrimitiveType x = case x of
    ConT n          -> (=='#') . last . nameBase $ n
    TupleT _        -> True
    UnboxedTupleT _ -> True
    ListT           -> True
    _               -> False

traceItNote name x = trace (name ++ " " ++ show x) x

--I think this pretty error prone. Not sure if String should really be
--in here...

defaultPrimitives :: [Name]
defaultPrimitives = [
    ''Int         ,
    ''Char        ,
    ''String      ,
    ''Float       ,
    ''Double      ,
    ''Int8        ,
    ''Int16       ,
    ''Int32       ,
    ''Int64       ,
    ''Word        ,
    ''Word8       ,
    ''Word16      ,
    ''Word32      ,
    ''Word64      ,
    ''Integer     ,
    ''NameFlavour  ]

isPrimitive :: Name -> Bool
isPrimitive = flip elem defaultPrimitives
-- TODO rename hardness
toConHardness :: TH.Type -> FamConType
toConHardness x = case x of
    ConT name 
        | isPrimitive name -> Abstract
        | otherwise        -> Concrete
    _ -> Concrete


typToString :: TH.Type -> String
typToString x = case x of
    ForallT _ _ typ -> typToString typ
    AppT a b        -> typToString a ++ typToString b
    ConT n          -> prettifyName n
    TupleT c        -> "Tuple" ++ show c
    UnboxedTupleT c -> "UnboxedTupleT" ++ show c
    ListT           -> "List"
    _               -> error $ "Unsupported type in " ++ show x

prettifyName :: Name -> String
prettifyName n 
    | n == '(:)  = "Cons"
    | n == '[] = "Nils"
    | nameBase n == "()" = "Unit"
--    | isTuple $ nameBase n = "Tuple" 
    | otherwise          = nameBase n

--TODO need to fix this
toNewName :: String -> TH.Type -> Name -> Q Name
toNewName prefix typ n = return . mkName $ 
        filter (\x -> x /= '[' && x /= ']') $ prefix ++ 
           typToString typ ++ (prettifyName n) ++ "C"
    
getConName :: TH.Type -> Name
getConName x = case x of
    ConT n -> n
    _      -> error $ "getConName used on " ++ show x

toFamCon :: String -> TH.Type -> (Maybe TH.Con) -> Q FamCon
toFamCon prefix typ x = do
    case x of
        Just con -> do
           let (n, fields) = getNameAndFields con 
           newN <- toNewName prefix typ n  
           return $ FamCon Concrete newN n fields 
        Nothing -> do 
           let n = getConName typ
           newN <- toNewName prefix typ n
           return $ FamCon Abstract newN n [typ]

getNameAndFields :: TH.Con -> (Name, [TH.Type])
getNameAndFields con = case con of
    NormalC n stys  -> (n, map snd stys)
    RecC n vtys     -> (n, map (\(_, _, z) -> z) vtys)
    InfixC x n y    -> (n, [snd x, snd y])
    ForallC _ _ con -> getNameAndFields con
    
toFamType :: String -> (TH.Type, Dec) -> Q FamType
toFamType prefix (t, x) = case x of
    DataD _ _ _ cons _   -> toFamType' prefix t cons
    NewtypeD _ _ _ con _ -> toFamType' prefix t [con]
    TySynD _ _ _         -> error $ "Logic error: all type declarations should be "++
                                         "converted to DataDecl or NewtypeDec"
    x                    -> error $ "unsuppored Dec: " ++ show x

toFamType' :: String -> TH.Type -> [TH.Con] -> Q FamType
toFamType' prefix typ cons = do 
    let hardness = toConHardness typ
    let consOrType = case hardness of
                        Concrete -> map Just cons
                        Abstract -> [Nothing] 
    FamType typ <$> mapM (toFamCon prefix typ) consOrType 

toFam :: String -> Name -> [(TH.Type, Dec)] -> Q Fam
toFam prefix name decs = Fam name <$> mapM (toFamType prefix) decs 

--TODO make this take a renameFunctions: for the family, for the constructors
makeGDiff :: Name -> Q [Dec]
makeGDiff name = do
    let familyName = mkName $ nameBase name ++ "Family"
        prefix     = nameBase name
        
    fam       <- toFam prefix familyName =<< specialize name
    instances <- mkAllInstances fam
    gadt      <- mkGADT         fam 

    return $ gadt : instances

