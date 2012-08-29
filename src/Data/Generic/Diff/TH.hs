{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, QuasiQuotes, GADTs,
    FlexibleContexts, FlexibleInstances, NoMonomorphismRestriction, GeneralizedNewtypeDeriving #-}
module Data.Generic.Diff.TH (
    -- ** Main Interface
    make_family_gadt, 
    -- ** Utils
    collect_type_args) where
import Language.Haskell.TH
import Control.Applicative
import Data.List
import Control.Monad
import Data.Generic.Diff ((:=:) (..), Nil (..), Cons (..))
-- import Language.Haskell.TH.ExpandSyns
import Language.Haskell.TH.TypeSub
import Language.Haskell.TH.Universe
import Language.Haskell.TH.Specialize (expand_and_specialize, expand_and_specialize_syns)
import Data.Tuple.Select
import Control.Monad.Error
import Control.Monad.State
import Control.Monad.Reader

collect_type_args :: Type -> [Type]
collect_type_args (AppT x y) = x:(collect_type_args y)
collect_type_args x          = [x]

run_state x xs = runReaderT (runErrorT (runErrorStateT x)) xs

type ERType m e r a = ErrorT e (ReaderT r m) a

newtype ERT e r m a = ERT { runErrorStateT :: ERType m e r a }
    deriving (Monad, MonadError e, Functor, MonadPlus, MonadReader r)
    
instance MonadTrans (ERT String [Dec]) where
    lift = ERT . lift . lift 

type DecState = ERT String [Dec] Q

--------------------------------------------------------

mk_specialized_universe :: Name -> Q [Dec]
mk_specialized_universe name = do 
    first_universe       <- (map snd . filter ((/=name) . fst)) <$> get_universe name
    specialized_universe <- expand_and_specialize name name
    let combined_universe = map snd $ sub_universe (first_universe ++ specialized_universe) name
    return combined_universe


-- | Pass in the name of to generate the GDiff GADT and instances. 
make_family_gadt :: Name -> Q [Dec]
make_family_gadt name = do    
    result <- run_state (make_family_gadt' name) =<< mk_specialized_universe name
    case result of 
        Right x -> return x
        Left x -> do
                     error x
                     return []

is_primitive name = any ((nameBase name) ==) ["Int",
 "Char", "String", "Float", 
 "Double", "Int8", "Int16", "Int32",
 "Int64", "Word", "Word8", 
 "Word16", "Word32", "Word64", "Addr"]

convert_to_gadt_constrs (name, cons) = map (mk_gadt_con name) cons

mk_gadt_con :: Name -> Con -> Con
mk_gadt_con name (NormalC c_name stys) = mk_gadt_con' name c_name $ map snd stys 
mk_gadt_con name (RecC    c_name vtys) = mk_gadt_con' name c_name $ map sel3 vtys 
mk_gadt_con name (InfixC  (_, x) c_name (_, y)) = mk_gadt_con' name c_name [x, y]


mk_gadt_con' name c_name tys | is_primitive name = ForallC [] [EqualP (VarT $ mkName "a") $ 
    ConT name, EqualP (VarT $ mkName "b") $ ConT $ mkName "Nil" ]
    (NormalC (con_name_to_display (nameBase name) name) [(NotStrict, ConT $ name)])
mk_gadt_con' name c_name tys | otherwise = ForallC [] [EqualP (VarT $ mkName "a") $ ConT name, 
    EqualP (VarT $ mkName "b") $ foldr AppT (ConT $ mkName "Nil") (add_cons tys) ]
    (NormalC (con_name_to_display (nameBase c_name) name) []) 

con_name_to_display c_name name  | c_name == "[]" = mkName $ "Nil" ++ show name ++ "'"
con_name_to_display c_name name  | c_name == ":" = mkName $ show name ++ "__Cons'"
con_name_to_display c_name _ | otherwise = mkName $ c_name ++ "'"

display_to_con_name display_name name | display_name == "Nil" ++ show name     = mkName $ "[]"
display_to_con_name display_name name | display_name == (show name ++ "__Cons") = mkName $ ":"
display_to_con_name display_name _    | otherwise = mkName display_name

add_cons tys = map (AppT (ConT $ mkName "Cons")) tys 

mk_gadt :: Name -> [Con] -> DecState Dec
mk_gadt gadt_name gadt_constrs = return $ DataD [] gadt_name [PlainTV $ mkName "a", PlainTV $ mkName "b"] 
    gadt_constrs []


is_dec_name name x = result where
    dec_name = get_dec_name x
    result =  case dec_name of 
                Right y -> name == y
                Left _  -> False   

make_family_gadt' :: Name -> DecState [Dec]
make_family_gadt' name = do
    first_universe       <- (map snd . filter ((/=name) . fst)) <$> (lift $ get_universe name)
    specialized_universe <- (filter (not . is_dec_name name)) <$> 
                                (lift $ expand_and_specialize_syns name name)
    
    all_constructor <- get_cons_from_name
    let gadt_constrs     = nub $ concatMap convert_to_gadt_constrs all_constructor
        gadt_name        = mkName $ (nameBase name) ++ "Fam"
    data_dec        <- mk_gadt gadt_name gadt_constrs
    family_instance <- mk_family_instance data_dec
    type_instances  <- mk_type_instances gadt_name $ nub all_constructor

    return $ data_dec:family_instance:(specialized_universe ++ type_instances)


is_right (Right _) = True
is_right _ = False

get_dec_name :: Dec -> Result Name
get_dec_name (FunD name _)             = Right $ name
get_dec_name (ValD _ _ _)              = Left "ValD does not have a name"
get_dec_name (DataD _ name _ _ _)      = Right $ name
get_dec_name (NewtypeD _ name _ _ _)   = Right $ name
get_dec_name (TySynD name _ _)         = Right $ name
get_dec_name (ClassD _ name _ _ _)     = Right $ name
get_dec_name (InstanceD _ _ _)         = Left "InstanceD does not have a name"
get_dec_name (SigD name _)             = Right $ name
get_dec_name (ForeignD _)              = Left "ForeignD does not have a name"
get_dec_name (PragmaD _ )              = Left "PragmaD does not have a name"
get_dec_name (FamilyD _ name _ _)      = Right $ name
get_dec_name (DataInstD _ name _ _ _ ) = Right $ name

get_name_and_cons :: [Dec] -> [(Name, [Con])]
get_name_and_cons decs = go decs [] where
    go [] output = output
    go (x:xs) output = result where
        name_result = get_dec_name x
        cons = get_cons x
        result = case name_result of
                    Right x -> (x, cons):(go xs output)
                    _      -> go xs output

get_cons_from_name :: DecState [(Name, [Con])]
get_cons_from_name = do 
    decs <- ask 
    return $ get_name_and_cons decs

find_dec :: [Dec] -> Name -> Result Dec
find_dec decs name = maybe_to_either ("could not find dec " ++ show name ++ " in " ++ show decs) $ find (is_dec_name name) decs


maybe_to_either :: String -> Maybe a -> Result a
maybe_to_either msg (Just a) = Right a
maybe_to_either msg Nothing = Left msg

throw_either :: (Monad m, MonadError String m) => Result a -> m a
throw_either (Right x) = return x
throw_either (Left x)  = throwError x


--------------------------------------------------------------------------------------------------

mk_family_instance :: Dec -> DecState Dec    
mk_family_instance (DataD _ name _ cons _) = do
    dec_eqs    <- mapM mk_deceq cons
    fields     <- mapM mk_field cons
    --todo add the default cases
    applys     <- mapM mk_apply cons
    strings    <- lift $ mapM mk_string cons
    let dec_fun    = FunD (mkName "decEq")  (dec_eqs ++ [default_dec]) 
        fields_fun = FunD (mkName "fields") (fields ++ [default_field])
        apply_fun  = FunD (mkName "apply")  applys
        string_fun = FunD (mkName "string") strings 

        dec  = InstanceD [] (AppT (ConT $ mkName "Family") (ConT name)) 
            [dec_fun, fields_fun, apply_fun, string_fun]
    return dec
    
default_dec = Clause [WildP, WildP] (NormalB $ ConE $ mkName "Nothing") []   

mk_deceq :: Con -> DecState Clause
mk_deceq (ForallC _ _ (NormalC name []))   = lift $ clause [conP name [], conP name []] (normalB [| Just (Refl, Refl) |]) []   
mk_deceq (ForallC _ _ (NormalC name typs)) = lift $
    clause [conP name [varP $ mkName "x"], conP name [varP $ mkName "y"]] 
    (normalB [| if $(varE $ mkName "x") == $(varE $ mkName "y") then Just (Refl, Refl) else Nothing |]) []

default_field = Clause [WildP, WildP] (NormalB $ ConE $ mkName "Nothing") []   
default_apply = Clause [WildP, WildP] (NormalB $ (AppE (VarE $ mkName "error") (LitE $ StringL "apply failed"))) []   

mk_field :: Con -> DecState Clause
mk_field (ForallC _ (x:y:[]) (NormalC c_name [])) = do 
    pat  <- name_to_con_pat (mkName $ reverse $ tail $ reverse (show c_name)) $ get_pred_name  x 
    let vars = collect_vars pat
    lift $ clause [conP c_name [], return pat] (normalB [| Just $(ccons_vars (reverse vars) [| CNil |]) |]) []
mk_field (ForallC _ _ (NormalC name typs)) = lift $ clause [conP name [wildP], wildP] (normalB [| Just CNil |]) []   

get_pred_name (EqualP _ (ConT n))  = n

name_to_con_pat :: Name -> Name -> DecState Pat
name_to_con_pat con_name n = do
    decs <- ask
    dec <- throw_either $ find_dec decs n
    data_dec_to_con_p con_name dec

get_con_name :: Con -> Name
get_con_name (NormalC n _)     = n
get_con_name (RecC n _)        = n
get_con_name (InfixC _ n _)    = n
get_con_name (ForallC _ _ con) = get_con_name con

is_nil_list con_name name typ | "Nil" `isPrefixOf` show con_name = True
is_nil_list con_name name typ | otherwise = False

reduce_type = undefined
reduce_type' = undefined

head_either []    = Left "Called head on empty list"
head_either (x:_) = Right x

data_dec_to_con_p :: Name -> Dec -> DecState Pat     
data_dec_to_con_p display_name x = do
     name <- throw_either $ get_dec_name x
     let con_name = display_to_con_name (nameBase display_name) name
     let cons = get_cons x
     let con_result = head_either $ 
                filter ((show con_name ==) . nameBase . get_con_name) cons     
     con <- case con_result of 
                Left _ -> throwError $ "data_dec_to_con_p failed with con_name = " ++ show con_name 
                                ++ " and x " ++ show x
                Right x -> return x 
            
     return $ con_to_con_p con 


rec_to_normal (RecC name vts) = NormalC name $ map (\(_, x, y) -> (x, y)) vts


con_to_con_p :: Con -> Pat
con_to_con_p x = result where
    name = get_con_name x
    typs = get_con_types x
    result = ConP name $ concat $ zipWith type_to_pat (map (:[]) ['a'..]) typs

type_to_pat var_preface (AppT ListT x) = 
    [InfixP (VarP $ mkName (var_preface)) (mkName ":") (VarP $ mkName (var_preface ++ "s"))]
type_to_pat var_preface (VarT x) = [VarP $ mkName var_preface]     
type_to_pat var_preface (AppT (ConT x) y) = [ConP x $ type_to_pat (var_preface ++ "_a") y]
type_to_pat var_preface (ConT x) = [(VarP $ mkName var_preface)]

collect_vars (ConP _ pvars) = concatMap collect_vars pvars 
collect_vars (InfixP x _ y) = concatMap collect_vars [x, y] 
collect_vars (VarP x)       = [x]
collect_vars (ListP [])     = []


ccons_vars [] e = e
ccons_vars (x:xs) e = ccons_vars xs [| CCons $(varE x) $(e) |]

mk_apply :: Con -> DecState Clause
mk_apply (ForallC _ (x:y:[]) (NormalC name [])) = do
    ex <- name_to_con_exp (mkName $ reverse $ tail $ reverse (show name)) $ get_pred_name x 
    let vars = collect_vars_exp ex
    lift $ clause [conP name [], ccons_vars_pat (reverse vars) $ conP (mkName "CNil") []]  
        (normalB (return ex)) []

mk_apply (ForallC _ _ (NormalC name typs)) = lift $ clause [conP name [varP $ mkName "x"], 
    conP (mkName "CNil") []] (normalB $ varE $ mkName "x") []

name_to_con_exp :: Name -> Name -> DecState Exp
name_to_con_exp con_name n = do
    decs <- ask
    dec <- throw_either $ find_dec decs n
    data_dec_to_con_exp con_name dec

    
data_dec_to_con_exp :: Name -> Dec -> DecState Exp    
data_dec_to_con_exp display_name dec = do
    name <- throw_either $ get_dec_name dec
    let con_name = display_to_con_name (nameBase display_name) name
        cons = get_cons dec
        con_result = head_either $ 
                        filter (((nameBase con_name)==) . nameBase . get_con_name) cons
                
    con <- case con_result of 
               Left _ -> throwError $ "data_dec_to_con_p failed with con_name = " ++ show display_name 
                               ++ " and x " ++ show dec
               Right x -> return x
        
    return $ con_to_con_e con 



collect_vars_exp (AppE x y) = collect_vars_exp x ++ collect_vars_exp y
collect_vars_exp (VarE x)   = [x]
collect_vars_exp (ListE xs) = concatMap collect_vars_exp xs
collect_vars_exp (ConE _)   = []
collect_vars_exp (InfixE (Just (VarE x)) _ (Just (VarE y)))   = [x, y]

ccons_vars_pat []     p = p
ccons_vars_pat (x:xs) p = ccons_vars_pat xs (conP (mkName "CCons") [varP x, p])

con_to_con_e :: Con -> Exp
con_to_con_e con = result where
    name = get_con_name con
    typs = get_con_types con
    result = foldl' AppE (ConE name) $ zipWith type_to_exp (map (:[]) ['a'..]) typs

type_to_exp var_preface (AppT ListT x) = 
    InfixE (Just $ VarE $ mkName (var_preface)) (ConE $ mkName ":") 
           (Just $ VarE $ mkName (var_preface ++ "s")) 
type_to_exp var_preface (VarT x) = VarE $ mkName var_preface
type_to_exp var_preface (AppT x y) = AppE (type_to_exp (var_preface ++ "_a") x) 
    (type_to_exp (var_preface ++ "_b") y)
type_to_exp var_preface (ConT x) = VarE $ mkName var_preface

mk_string :: Con -> Q Clause    
mk_string (ForallC _ _ (NormalC name []))   = 
    clause [conP name []] (normalB $ stringE $ reverse $ tail $ reverse $ show $ name) []

mk_string (ForallC _ _ (NormalC name typs)) = 
    clause [conP name [varP $ mkName "x"]] (normalB $ appE (varE $ mkName "show") $ 
        varE $ mkName "x") []

---------------------------------------------------------------------------------------------    

mk_type_instances gadt_name cons = mapM (mk_type_instance gadt_name) cons

mk_type_instance gadt_name (name, cons) = do
    let cons_exps = map (make_const name) cons
    return $ InstanceD [] (AppT (AppT (ConT $ mkName "Type") (ConT gadt_name)) (ConT name)) [
                FunD (mkName "constructors") [Clause [] (NormalB $ ListE cons_exps) []]]


make_const name con | is_primitive name = AppE (ConE $ mkName "Abstr") (ConE $ 
    con_name_to_display (nameBase $ name) name)
make_const name con | otherwise    = AppE (ConE $ mkName "Concr") (ConE $ 
    con_name_to_display (nameBase $ get_con_name con) name) 






