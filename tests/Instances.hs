{-# LANGUAGE TemplateHaskell, QuasiQuotes, TypeSynonymInstances, 
 GADTs, ExistentialQuantification, MultiParamTypeClasses, FlexibleInstances #-}
module Instances where
import Data.Generic.Diff.TH
import Data.Generic.Diff
import Language.Haskell.TH
import Language.Haskell.TH.Universe    
import Data.Word
import Data.Aeson
import Language.C
import Hdis86
import Language.ECMAScript3

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

--makeGDiff ''SumInTheType

data TypeA = AB TypeB Char
           
data TypeB = TypeB [Float]

data TypeC = TypeC

--makeGDiff ''TypeA

newtype UnitTest = UnitTest ()
--makeGDiff ''UnitTest

newtype IntSubbedType = IntSubbedType (SubbedType Int)

newtype NullJ = NullJ (JavaScript ())
--
--makeGDiff ''IntSubbedType
--makeGDiff ''CTranslUnit
--makeGDiff ''Instruction
--makeGDiff ''NullJ

-- makeGDiff ''Value

--data Tree a = Leaf a | Node (Tree (a,a))
--newtype WordTree = WordTree (Tree Word32)
--makeGDiff ''WordTree

-- $(makeGDiff ''TupleTest)


$(do
    --info <- specialize ''TypeA
    --runIO $ print info    

    --info <- reify ''()
    --runIO $ print info
--    univ <- getUniverse ''DoubleList
--    runIO $ print univ

    return [])


--makeGDiff ''UnpolyTest
--makeGDiff ''ListLike
--makeGDiff ''DoubleList
--makeGDiff ''Info

