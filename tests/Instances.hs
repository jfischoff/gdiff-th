{-# LANGUAGE TemplateHaskell, QuasiQuotes, TypeSynonymInstances, 
 MultiParamTypeClasses, FlexibleInstances #-}
module Instances where
import Data.Generic.Diff.TH
import Data.Generic.Diff
    
type IntList = [Int]

data ListLike = ListLike
    {
        values :: IntList
    }
    deriving(Show, Eq)


$(make_family_gadt ''ListLike)