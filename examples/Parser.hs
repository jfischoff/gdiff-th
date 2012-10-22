{-# LANGUAGE NoMonomorphismRestriction, GADTs, FlexibleContexts,
    TemplateHaskell, LiberalTypeSynonyms, MultiParamTypeClasses,
    DeriveDataTypeable #-}
module Parser where
import Expr
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
         
--Pretty printer
ppr :: Exp -> String
ppr e = case e of
   x :+: y -> ppr x ++ " + " ++ ppr y
   x :*: y -> ppr x ++ " * " ++ ppr y
   B x     -> show x

-- parser    
badParser :: String -> Either ParseError Exp                     
badParser x = runParser pExp () "" x where
    pExp = foldl chainr1 (parens pExp <|> pInt) $ 
                    map binOp [("+", (:+:)), ("*", (:*:))] 

    binOp (x, rest) = do
        spaces
        P.string x
        spaces
        return rest

    pInt = B <$> P.integer P.haskell

    parens = P.parens P.haskell
   
deriveEnumerable ''Exp 

-- A very important variant that will fail
pprToParseRoundTrip :: Exp -> Bool
pprToParseRoundTrip x = either (const False) (x ==) $ (badParser . ppr) x

checkParser = featCheck 12 pprToParseRoundTrip

-- A convienent hack for readablility       
instance Num Exp where
    (+) = (:+:)
    (*) = (:*:)
    fromInteger = B
    abs    = undefined
    signum = undefined

badParserBug = (*) ((*) 0 0) 0

fromRight' (Right x) = x

badParserDiff = showEdits $ diffExp badParserBug (fromRight' . badParser . ppr $ badParserBug) 

-- the fixed parser 
goodParser :: String -> Either ParseError Exp                     
goodParser x = runParser pExp () "" x where
    pExp = foldl chainl1 (parens pExp <|> pInt) $ 
                    map binOp [("+", (:+:)), ("*", (:*:))] 

    binOp (x, rest) = do
        spaces
        P.string x
        spaces
        return rest

    pInt = B <$> P.integer P.haskell

    parens = P.parens P.haskell





