Generate GDiff GADTs and Associated Instances
=============
gdiff-th is a library for generating the necessary apparatus for preforming type safe diffs and patches with the gdiff library. 

If you are unfamiliar with the gdiff library it can be found here http://hackage.haskell.org/package/gdiff

###Usage

First install the library from here or from cabal.
#####With git
```bash
    git clone https://github.com/jfischoff/gdiff-th
    cd gdiff-th
    cabal install
```

#####With cabal 
```bash
    cabal update
    cabal install gdiff-th
```

Below is a simple example of how to use the library to view a colored diff.
```haskell
    module Example where
    import Data.Generic.Diff  
    import Data.Generic.Diff.TH  
    import System.Console.Terminfo.Color
    import Text.PrettyPrint.Free hiding (parens)
    import System.Console.Terminfo.PrettyPrint

    data Exp = Exp :+: Exp
             | Exp :*: Exp
             | B Integer
             deriving(Show, Eq, Typeable)

    -- Make the GDiff apparatus
    makeGDiff ''Exp

    testA :: Exp
    testA = foldl1 (:+:) . map B $ [0..20]

    testB :: Exp
    testB = foldl1 (:+:) . map B $ [0..8] ++ [42] ++ [10..20]

    -- Make a type signature to help inference
    diffExp :: Type ExpFamily Exp => Exp -> Exp -> EditScript ExpFamily Exp Exp
    diffExp = diff

    diffAandB = showCompressed $ diffExp testA testB  

    main = diffAandB

    -- Utility functions to show colored diffs
    showEdits :: forall (f :: * -> * -> *) txs tys.
                       EditScriptL f txs tys -> IO ()
    showEdits      = display . pprEdits 

    showCompressed :: Family f => EditScriptL f txs tys -> IO ()
    showCompressed = display . pprEdits . compress

    pprEdits :: EditScriptL f txs tys -> TermDoc
    pprEdits x = case x of 
        Cpy c d   -> (text $ string c) + pprEdits d
        CpyTree d -> text " ... "      + pprEdits d
        Del c d   -> (with (Foreground Red)   . text $ "- " ++ string c) + pprEdits d
        Ins c d   -> (with (Foreground Green) . text $ "+ " ++ string c) + pprEdits d
        End       -> line
```

Running the main function above would result in the following output
        
        :+: :+: :+: :+: :+: :+: :+: :+: :+: :+: :+: :+: ... B + 42 - 9 ... ... ... ... ... ... ... ... ... ... ... 

Except with pretty colors :).   
 