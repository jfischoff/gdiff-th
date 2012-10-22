-- | This module exports the Template Haskell functions necessary 
--   deriving gdiff GADTs and associated instances. Usage is pretty 
--   straightforward.
--
-- @
--module Example where
--import "Data.Generic.Diff"  
--import "Data.Generic.Diff.TH"  
--import "System.Console.Terminfo.Color"
--import "Text.PrettyPrint.Free hiding (parens)"
--import "System.Console.Terminfo.PrettyPrint"
--    
--data Exp = Exp :+: Exp
--         | Exp :*: Exp
--         | B Integer
--         deriving(Show, Eq, Typeable)
--
--{- Make the GDiff apparatus -}
--makeGDiff ''Exp
--  
--testA :: Exp
--testA = foldl1 (:+:) . map B $ [0..20]
--
--testB :: Exp
--testB = foldl1 (:+:) . map B $ [0..8] ++ [42] ++ [10..20]
--
--{- Make a type signature to help inference -}
--diffExp :: Type ExpFamily Exp => Exp -> Exp -> EditScript ExpFamily Exp Exp
--diffExp = diff
--
--diffAandB = showCompressed $ diffExp testA testB  
--
--main = diffAandB
--
--{- Utility functions to show colored diffs -}
--showEdits :: forall (f :: * -> * -> *) txs tys.
--                   EditScriptL f txs tys -> IO ()
--showEdits      = display . pprEdits 
--
--showCompressed :: Family f => EditScriptL f txs tys -> IO ()
--showCompressed = display . pprEdits . compress
--
--pprEdits :: EditScriptL f txs tys -> TermDoc
--pprEdits x = case x of 
--    Cpy c d   -> (text $ string c) <+> pprEdits d
--    CpyTree d -> text \" ... \"      <+> pprEdits d
--    Del c d   -> (with (Foreground Red)   . text $ \"- \" ++ string c) <+> pprEdits d
--    Ins c d   -> (with (Foreground Green) . text $ \"+ \" ++ string c) <+> pprEdits d
--    End       -> line
-- @
--
--  Running the main function above would result in the following output
-- @
--:+: :+: :+: :+: :+: :+: :+: :+: :+: :+: :+: :+:  ...  B + 42 - 9  ...   ...   ...   ...   ...   ...   ...   ...   ...   ...   ...  
-- @
--  Except with pretty colors 
module Data.Generic.Diff.TH (
    -- * Main Creation Function
    makeGDiff, 
    -- * Customizable Creation
    makeGDiffWith, 
    defaultFamSuffix,
    defaultConstructorRenamer, 
    defaultPrimitives, 
    ConstructorRenamer) where
import Data.Generic.Diff.TH.Internal
