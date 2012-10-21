{-# LANGUAGE GADTs, KindSignatures, RankNTypes #-}
module Utils where
import Data.Generic.Diff   -- (EditScript(..), diff, Type, compress)
import System.Console.Terminfo.Color
import Text.PrettyPrint.Free hiding (parens)
import System.Console.Terminfo.PrettyPrint

showEdits :: forall (f :: * -> * -> *) txs tys.
                   EditScriptL f txs tys -> IO ()
showEdits      = display . pprEdits 

showCompressed :: Family f => EditScriptL f txs tys -> IO ()
showCompressed = display . pprEdits . compress

pprEdits :: EditScriptL f txs tys -> TermDoc
pprEdits x = case x of 
    Cpy c d   -> (text $ string c) <+> pprEdits d
    CpyTree d -> text " ... "      <+> pprEdits d
    Del c d   -> (with (Foreground Red)   . text $ "- " ++ string c) <+> pprEdits d
    Ins c d   -> (with (Foreground Green) . text $ "+ " ++ string c) <+> pprEdits d
    End       -> line