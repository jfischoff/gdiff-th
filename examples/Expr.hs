{-# LANGUAGE NoMonomorphismRestriction, GADTs, FlexibleContexts,
    TemplateHaskell, LiberalTypeSynonyms, MultiParamTypeClasses,
    DeriveDataTypeable #-}
module Expr where
import Text.Parsec hiding ((<+>), string)
import qualified Text.Parsec as P    
import qualified Text.Parsec.Token as P
import qualified Text.Parsec.Language as P
import Control.Applicative ((<$>))
import Data.Generic.Diff.TH
import Data.Generic.Diff   -- (EditScript(..), diff, Type, compress)
import System.Console.Terminfo.Color
import Text.PrettyPrint.Free hiding (parens)
import System.Console.Terminfo.Base
import System.Console.Terminfo.PrettyPrint
import Test.Feat
import Data.Typeable
import Utils

-- A simple Expression

data Exp = Exp :+: Exp
         | Exp :*: Exp
         | B Integer
         deriving(Show, Eq, Typeable)
         
makeGDiff ''Exp
         
-- Two examples using the num hack    
testA :: Exp
testA = foldl1 (:+:) . map B $ [0..20]

testB :: Exp
testB = foldl1 (:+:) . map B $ [0..8] ++ [42] ++ [10..20]   
    
-- For some reason I seem to need to do this to help out type inference
diffExp :: (Type ExpFamily Exp) 
        => Exp -> Exp -> EditScript ExpFamily Exp Exp
diffExp = diff

diffAandB = showCompressed $ diffExp testA testB  

main = diffAandB       
         
-- Now a more practical example
