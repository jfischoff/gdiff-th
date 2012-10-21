{-# LANGUAGE TemplateHaskell, QuasiQuotes, TypeSynonymInstances, 
 GADTs, ExistentialQuantification, MultiParamTypeClasses, FlexibleInstances #-}
module Instances where
import Data.Generic.Diff ()
import Data.Generic.Diff.TH
import Language.Haskell.TH
import Hdis86
import Language.ECMAScript3
import Language.Haskell.TH.Syntax (NameFlavour)


type IntList = [Int]

data ListLike = ListLike
    {
        values :: IntList
    }
    deriving(Show, Eq)

data DoubleList = DoubleList 
    {
        values1 :: [Int],
        values2 :: ([Float], Double)
    }

data PolyTest a = PolyTest 
	{
		best :: [a]
	}

data UnpolyTest = UnpolyTest {
		unpoly :: PolyTest Char
	}

data TupleTest = TupleTest {
    tt :: (Int, Double)
}

data SubbedType a = SubbedType {
        subbedType :: PolyTest a
    }

data SumType = A | B | C

data SumInTheType = SumInTheType {
        sitt :: SumType
    }

makeGDiff ''SumInTheType

data TypeA = AB TypeB Char
           
data TypeB = TypeB [Float]

data TypeC = TypeC
makeGDiff ''TypeA

newtype UnitTest = UnitTest ()
makeGDiff ''UnitTest

newtype IntSubbedType = IntSubbedType (SubbedType Int)

newtype NullJ = NullJ (JavaScript ())

makeGDiff ''Instruction
makeGDiff ''NullJ
makeGDiff ''UnpolyTest
makeGDiff ''ListLike
makeGDiff ''DoubleList
makeGDiffWith defaultFamSuffix defaultConstructorRenamer 
    ((''NameFlavour, LamE [WildP] . LitE . StringL $ "") : defaultPrimitives) ''Info

