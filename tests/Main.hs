{-# LANGUAGE TemplateHaskell, QuasiQuotes, TypeSynonymInstances, 
 MultiParamTypeClasses, FlexibleInstances, FlexibleContexts #-}
module Main where
import Data.Generic.Diff.TH
import Data.Generic.Diff
import Instances

data Poly a = Poly
    {
        value :: a,
        name :: String
    }
    deriving(Show, Eq)
    
type PolyInt = Poly Int

-- $(make_family_gadt ''PolyInt)


diff_list_like :: (Type ListLikeFam ListLike) => ListLike -> ListLike -> 
    EditScript ListLikeFam ListLike ListLike
diff_list_like x y = diff x y

main = do
    print $ compress $ diff_list_like (ListLike [1,2,3,4,5,6,7,8,9]) 
                            (ListLike [1,2,3,4,5,6,7,8,10])