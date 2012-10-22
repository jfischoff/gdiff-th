{-# LANGUAGE TemplateHaskell, QuasiQuotes, TypeSynonymInstances, FlexibleContexts,
 GADTs, ExistentialQuantification, MultiParamTypeClasses, FlexibleInstances #-}
module Main where
import Language.Haskell.Exts (parseFile, Module, ParseResult(..), fromParseResult)
import Data.Generic.Diff.TH
import Data.Generic.Diff 
import Control.Applicative
import Utils

makeGDiff ''Module

diffModule :: (Type ModuleFamily Module) 
        => Module -> Module -> EditScript ModuleFamily Module Module
diffModule = diff

main = do 
    old <- fromParseResult <$> parseFile "examples/Old.hs"
    new <- fromParseResult <$> parseFile "examples/New.hs"
    showCompressed $ diffModule old new

        
    