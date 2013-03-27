{-# LANGUAGE TemplateHaskell, GeneralizedNewtypeDeriving, StandaloneDeriving,
    QuasiQuotes, TypeSynonymInstances, FlexibleContexts,
 GADTs, ExistentialQuantification, MultiParamTypeClasses, FlexibleInstances #-}
module Main where
import Language.Haskell.Exts as H
import Data.Generic.Diff.TH
import Data.Generic.Diff as G
import Control.Applicative
import Utils
import Control.DeepSeq
import Control.DeepSeq.TH

makeGDiff ''Module

diffModule :: (G.Type ModuleFamily Module) 
        => Module -> Module -> EditScript ModuleFamily Module Module
diffModule = diff

deriveNFData ''Module
deriveNFData ''Decl
deriveNFData ''ImportDecl
deriveNFData ''Annotation
deriveNFData ''ExportSpec
deriveNFData ''Activation
deriveNFData ''ImportSpec
deriveNFData ''WarningText
deriveNFData ''Rule
deriveNFData ''CName
deriveNFData ''RuleVar
deriveNFData ''Safety
deriveNFData ''ModulePragma
deriveNFData ''Tool
deriveNFData ''CallConv
deriving instance NFData ModuleName
deriveNFData ''SrcLoc
deriveNFData ''Binds
deriveNFData ''Rhs
deriveNFData ''Pat
deriveNFData ''IPBind
deriveNFData ''GuardedRhs
deriveNFData ''IPName
deriveNFData ''Match
deriveNFData ''PXAttr
deriveNFData ''Stmt
deriveNFData ''Exp
deriveNFData ''XName
deriveNFData ''Op
deriveNFData ''RPat
deriveNFData ''XAttr
deriveNFData ''Assoc
deriveNFData ''PatField
deriveNFData ''Splice
deriveNFData ''RPatOp
deriveNFData ''InstDecl
deriveNFData ''Literal
deriveNFData ''Bracket
deriveNFData ''ClassDecl
deriveNFData ''QualStmt
deriveNFData ''FunDep
deriveNFData ''FieldUpdate
deriveNFData ''GadtDecl
deriveNFData ''Alt
deriveNFData ''QName
deriveNFData ''QOp
deriveNFData ''GuardedAlts
deriveNFData ''QualConDecl
deriveNFData ''SpecialCon
deriveNFData ''GuardedAlt
deriveNFData ''Asst
deriveNFData ''ConDecl
deriveNFData ''Boxed
deriveNFData ''DataOrNew
deriveNFData ''BangType
deriveNFData ''Kind
deriveNFData ''H.Type
deriveNFData ''TyVarBind
deriveNFData ''Name

main = do 
    old <- fmap (force . fromParseResult) . parseFile $ "examples/Old.hs"
    
    new <- fmap (force . fromParseResult) . parseFile $ "examples/New.hs"
    showCompressed $ diffModule old new

        
    