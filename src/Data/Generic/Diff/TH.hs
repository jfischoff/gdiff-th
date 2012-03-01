module Data.Generic.Diff.TH (make_family_gadt) where
import Language.Haskell.TH
import Control.Applicative
import Data.List
import Control.Monad
import Data.Generic.Diff ((:=:) (..), Nil (..), Cons (..))
import Safe
import Debug.Trace
import Debug.Trace.Helpers
import Language.Haskell.TH.ExpandSyns
import Language.Haskell.TH.TypeSyn

traceItNote n x = trace (n ++ (show x)) x

reify_data_dec name = do
    (TyConI dec) <- reify name
    return dec

make_family_gadt :: Name -> Q [Dec]
make_family_gadt name = do
    all_constructor <- recursively_get_types name
    let gadt_constrs     = nub $ concatMap convert_to_gadt_constrs all_constructor
        gadt_name        = mkName $ (nameBase name) ++ "Fam"
    data_dec        <- mk_gadt gadt_name gadt_constrs
    family_instance <- mk_family_instance data_dec
    type_instances  <- mk_type_instances gadt_name $ nub all_constructor

    return $ data_dec:family_instance:type_instances

recursively_get_types :: Name -> Q [(Name, [Con])]
recursively_get_types name | is_primitive name = return [(name, [NormalC name []])]
recursively_get_types name | otherwise = do
    dec <- reify_data_dec $ traceIt name
    collect_constr dec

collect_constr :: Dec -> Q [(Name, [Con])]
collect_constr (DataD _ name _ cons _) = do
    children <- concat <$> mapM collect_constr_decs cons 
    return ((mkName $ nameBase name, cons):children)
collect_constr (TySynD name _ typ) | is_list typ = trace "is_list" $ mk_list_cons name typ
collect_constr (TySynD name _ typ) | otherwise = mk_typsym_cons name typ
--I should handle the list case here

to_name_cons :: Dec -> (Name, [Con])
to_name_cons (DataD _ n _ cs _) = (n, cs)
--traceItNote x y = trace (x ++ show y) y

set_dec_name name (DataD x _ y z w) = DataD x name y z w

mk_typsym_cons name typ = do
    (AppT (ConT n) args) <- traceIt <$> expandSyns typ
    collect_constr =<< (set_dec_name name <$> reduce_type n args)

typsym_typ_to_cons (AppT x y) = typsym_typ_to_cons x ++ typsym_typ_to_cons y
typsym_typ_to_cons x@(ConT _) = [(NotStrict, x)]
typsym_typ_to_cons x@(VarT _) = error "can't handle polymorphic types!"

mk_list_cons name (AppT _ x) = do 
    xs <- look_con_args x
    return ([(name, [NormalC (mkName ("Nil" ++ nameBase name)) [], 
                     NormalC (mkName $ nameBase name) 
                        [(NotStrict, x), (NotStrict, ConT name)]])] ++ xs)

is_list (AppT (ListT) _) = True
is_list _ = False

--the issue
--this doesn't handle polymorphic types right     

collect_constr_decs :: Con -> Q [(Name, [Con])]
collect_constr_decs (NormalC _ stys) = concat <$> (mapM look_con_args $ map (\(_, typ) -> typ) stys)
collect_constr_decs (RecC    _ vtys) = concat <$> (mapM look_con_args $ map (\(_, _, typ) -> typ) vtys)

look_con_args :: Type -> Q [(Name, [Con])]
look_con_args (ConT name) = recursively_get_types name
look_con_args (AppT _ (VarT y))  = return []
look_con_args (AppT (VarT x) _)  = return []
look_con_args (AppT x y)  = do
    xs <- look_con_args x
    ys <- look_con_args y
    return (xs ++ ys)
look_con_args (ListT)     = return []
look_con_args (VarT x)    = return []
look_con_args x = error ("look_con_args" ++ show x)

is_primitive name = any (\x -> isSuffixOf x (show name)) [".Int", ".Char", ".String"]

convert_to_gadt_constrs (name, cons) = map (mk_gadt_con name) cons

mk_gadt_con :: Name -> Con -> Con
mk_gadt_con name (NormalC c_name stys) = mk_gadt_con' name c_name $ map (\(_, typ) -> typ) stys 
mk_gadt_con name (RecC    c_name vtys) = mk_gadt_con' name c_name $ map (\(_, _, typ) -> typ) vtys 

mk_gadt_con' name c_name tys | is_primitive name = ForallC [] [EqualP (VarT $ mkName "a") $ ConT name, 
    EqualP (VarT $ mkName "b") $ foldl1' AppT ((add_cons tys) ++ [ConT $ mkName "Nil"]) ]
    (NormalC (mkName (nameBase c_name ++ "'")) [(NotStrict, ConT name)])
mk_gadt_con' name c_name tys | otherwise = ForallC [] [EqualP (VarT $ mkName "a") $ ConT name, 
    EqualP (VarT $ mkName "b") $ foldr AppT (ConT $ mkName "Nil") (add_cons tys) ]
    (NormalC (mkName (nameBase c_name ++ "'")) [])

add_cons tys = map (AppT (ConT $ mkName "Cons")) tys 

mk_gadt :: Name -> [Con] -> Q Dec
mk_gadt gadt_name gadt_constrs = return $ DataD [] gadt_name [PlainTV $ mkName "a", PlainTV $ mkName "b"] 
    gadt_constrs []


--------------------------------------------------------------------------------------------------------

mk_family_instance :: Dec -> Q Dec    
mk_family_instance (DataD _ name _ cons _) = do
    dec_eqs    <- mapM mk_deceq cons
    fields     <- mapM mk_field cons
    --todo add the default cases
    applys     <- mapM mk_apply cons
    strings    <- mapM mk_string cons
    let dec_fun    = FunD (mkName "decEq")  (dec_eqs ++ [default_dec]) 
        fields_fun = FunD (mkName "fields") (fields ++ [default_field])
        apply_fun  = FunD (mkName "apply")  applys
        string_fun = FunD (mkName "string") strings 

        dec  = InstanceD [] (AppT (ConT $ mkName "Family") (ConT name)) 
            [dec_fun, fields_fun, apply_fun, string_fun]
    return dec
    
default_dec = Clause [WildP, WildP] (NormalB $ ConE $ mkName "Nothing") []   

mk_deceq :: Con -> Q Clause
mk_deceq (ForallC _ _ (NormalC name []))    = clause [conP name [], conP name []] (normalB [| Just (Refl, Refl) |]) []   
mk_deceq (ForallC _ _ (NormalC name typs)) = 
    clause [conP name [varP $ mkName "x"], conP name [varP $ mkName "y"]] 
    (normalB [| if $(varE $ mkName "x") == $(varE $ mkName "y") then Just (Refl, Refl) else Nothing |]) []

default_field = Clause [WildP, WildP] (NormalB $ ConE $ mkName "Nothing") []   
default_apply = Clause [WildP, WildP] (NormalB $ (AppE (VarE $ mkName "error") (LitE $ StringL "apply failed"))) []   

mk_field :: Con -> Q Clause
mk_field (ForallC _ (x:y:[]) (NormalC c_name [])) = do 
    pat <- name_to_con_pat (mkName $ reverse $ tail $ reverse (show c_name)) $ get_pred_name  x 
    let vars = collect_vars pat
    clause [conP c_name [], return pat] (normalB [| Just $(ccons_vars (reverse vars) [| CNil |]) |]) []
mk_field (ForallC _ _ (NormalC name typs)) = clause [conP name [wildP], wildP] (normalB [| Just CNil |]) []   

get_pred_name (EqualP _ (ConT n))  = n

name_to_con_pat :: Name -> Name -> Q Pat
name_to_con_pat con_name n = do
    (TyConI dec) <- reify n
    data_dec_to_con_p con_name dec

get_con_name (NormalC name _) = name
get_con_name (RecC name _) = name

is_nil_list con_name name typ | "Nil" `isPrefixOf` show con_name = True
is_nil_list con_name name typ | otherwise = False

--reduce_type' (AppT (ConT n) args) = reduce_type n args

     

data_dec_to_con_p :: Name -> Dec -> Q Pat
data_dec_to_con_p con_name (TySynD name _ typ )   | is_nil_list con_name name typ = return $ ListP []
data_dec_to_con_p con_name t@(TySynD name _ typ ) | otherwise = do 
    new_type <- traceIt <$> expandSyns typ
    reduce new_type where
        
        reduce (AppT (ConT n) args) = data_dec_to_con_p con_name =<< reduce_type n args
        reduce ty = return $ head $ type_to_pat "x" ty
        
data_dec_to_con_p con_name (DataD _ name _ cons _) = return $ data_dec_to_con_p' con_name name cons 
    
data_dec_to_con_p' con_name name cons = result where
    result = con_to_con_p con 
    con = headNote "data_dec_to_con_p" $ filter (((nameBase con_name)==) . nameBase . get_con_name) cons 


rec_to_normal (RecC name vts) = NormalC name $ map (\(_, x, y) -> (x, y)) vts


con_to_con_p (NormalC name tys) = ConP name $ concat $ zipWith type_to_pat (map (:[]) ['a'..]) $ map snd tys
con_to_con_p x@(RecC _ _)       = con_to_con_p $ rec_to_normal x

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

mk_apply :: Con -> Q Clause
mk_apply (ForallC _ (x:y:[]) (NormalC name [])) = do
    ex <- name_to_con_exp (mkName $ reverse $ tail $ reverse (show name)) $ get_pred_name x 
    let vars = collect_vars_exp ex
    clause [conP name [], ccons_vars_pat (reverse vars) $ conP (mkName "CNil") []]  
        (normalB (return ex)) []

mk_apply (ForallC _ _ (NormalC name typs)) = clause [conP name [varP $ mkName "x"], 
    conP (mkName "CNil") []] (normalB $ varE $ mkName "x") []

name_to_con_exp :: Name -> Name -> Q Exp
name_to_con_exp con_name n = do
    (TyConI dec) <- reify n
    data_dec_to_con_exp con_name dec

data_dec_to_con_exp con_name (TySynD name _ typ ) | is_nil_list con_name name typ = return $ ListE []
data_dec_to_con_exp con_name (TySynD name _ typ ) | otherwise = do
--    type_to_exp "a" typ    
    new_type <- traceIt <$> expandSyns typ
    reduce new_type where 
        
        reduce (AppT (ConT n) args) = data_dec_to_con_exp con_name =<< reduce_type n args
        reduce ty = return $ type_to_exp "x" ty
    
data_dec_to_con_exp con_name (DataD _ name _ cons _) = do
    let result = con_to_con_e con 
        con = headNote "data_dec_to_con_exp" $ filter (((nameBase con_name)==) . nameBase . get_con_name) cons
    return result


collect_vars_exp (AppE x y) = collect_vars_exp x ++ collect_vars_exp y
collect_vars_exp (VarE x)   = [x]
collect_vars_exp (ListE xs) = concatMap collect_vars_exp xs
collect_vars_exp (ConE _)   = []
collect_vars_exp (InfixE (Just (VarE x)) _ (Just (VarE y)))   = [x, y]

ccons_vars_pat []     p = p
ccons_vars_pat (x:xs) p = ccons_vars_pat xs (conP (mkName "CCons") [varP x, p])

con_to_con_e (NormalC name tys) = 
    foldl' AppE (ConE name) $ zipWith type_to_exp (map (:[]) ['a'..]) $ 
                                        map snd tys
con_to_con_e x@(RecC _ _)       = con_to_con_e $ rec_to_normal x

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
    let cons_exps = map make_const cons
    return $ InstanceD [] (AppT (AppT (ConT $ mkName "Type") (ConT gadt_name)) (ConT name)) [
                FunD (mkName "constructors") [Clause [] (NormalB $ ListE cons_exps) []]]


make_const con | is_primitive $ get_con_name con = AppE (ConE $ mkName "Abstr") (ConE $ 
    mkName ((nameBase $ get_con_name con) ++ "'")) 
make_const con | otherwise    = AppE (ConE $ mkName "Concr") (ConE $ 
    mkName ((nameBase $ get_con_name con) ++ "'")) 













