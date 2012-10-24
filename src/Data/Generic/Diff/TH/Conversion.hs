{-# LANGUAGE TemplateHaskell, CPP #-}
{-# OPTIONS_GHC -pgmPcpphs  -optP--cpp #-}
module Data.Generic.Diff.TH.Conversion where
import Data.Generic.Diff.TH.Types
import qualified Language.Haskell.TH as TH
import Language.Haskell.TH
import Control.Applicative


isPrimitive :: [Name] -> Name -> Bool
isPrimitive primitives = flip elem primitives
-- TODO rename hardness
toConHardness :: [Name] -> TH.Type -> FamConType
toConHardness prims x = case x of
    ConT name 
        | isPrimitive prims name -> Abstract
        | otherwise              -> Concrete
    _ -> Concrete


typToString :: TH.Type -> String
typToString x = case x of
    ForallT _ _ typ -> typToString typ
    AppT a b        -> typToString a ++ typToString b
    ConT n          -> prettifyName n
    TupleT c        -> "Tuple" ++ show c
#if __GLASGOW_HASKELL__ > 700
    UnboxedTupleT c -> "UnboxedTupleT" ++ show c
#endif
    ListT           -> "List"
    _               -> error $ "Unsupported type in " ++ show x

prettifyName :: Name -> String
prettifyName n 
    | n == '(:)  = "Cons"
    | n == '[] = "Nils"
    | nameBase n == "()" = "Unit"
--    | isTuple $ nameBase n = "Tuple" 
    | otherwise          = nameBase n


getConName :: TH.Type -> Name
getConName x = case x of
    ConT n -> n
    _      -> error $ "getConName used on " ++ show x

toFamCon :: (Name -> Type -> Q Name) -> TH.Type -> (Maybe TH.Con) -> Q FamCon
toFamCon renamer typ x = do
    case x of
        Just con -> do
           let (n, fields) = getNameAndFields con 
           newN <- renamer n typ   
           return $ FamCon Concrete newN n fields 
        Nothing -> do 
           let n = getConName typ
           newN <- renamer n typ
           return $ FamCon Abstract newN n [typ]

getNameAndFields :: TH.Con -> (Name, [TH.Type])
getNameAndFields con = case con of
    NormalC n stys  -> (n, map snd stys)
    RecC n vtys     -> (n, map (\(_, _, z) -> z) vtys)
    InfixC x n y    -> (n, [snd x, snd y])
    ForallC _ _ innerCon -> getNameAndFields innerCon

toFamType :: [Name] -> (Name -> Type -> Q Name) -> (TH.Type, Dec) -> Q FamType
toFamType prims renamer (t, x) = case x of
    DataD _ _ _ cons _   -> toFamType' prims renamer t cons
    NewtypeD _ _ _ con _ -> toFamType' prims renamer t [con]
    TySynD _ _ _         -> error $ "Logic error: all type declarations should be "++
                                         "converted to DataDecl or NewtypeDec"
    e                    -> error $ "unsuppored Dec: " ++ show e

toFamType' :: [Name] -> (Name -> Type -> Q Name) -> TH.Type -> [TH.Con] -> Q FamType
toFamType' prims renamer typ cons = do 
    let hardness = toConHardness prims typ
    let consOrType = case hardness of
                        Concrete -> map Just cons
                        Abstract -> [Nothing] 
    FamType typ <$> mapM (toFamCon renamer typ) consOrType 

toFam :: [Name] -> (Name -> Type -> Q Name) -> Name -> [(TH.Type, Dec)] -> Q Fam
toFam prims renamer name decs = Fam name <$> mapM (toFamType prims renamer) decs
