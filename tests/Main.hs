{-# LANGUAGE TemplateHaskell, QuasiQuotes, TypeSynonymInstances, 
 MultiParamTypeClasses, FlexibleInstances, FlexibleContexts #-}
module Main where
import Data.Generic.Diff.TH ()
import Data.Generic.Diff ()
import Instances ()

-- great test huh?
-- Mostly I am making sure it is able to build "something" on large
-- expressions
-- As soon as I discover "a" bug, I will figure out how to test the TH code for real
main = print "hey"
