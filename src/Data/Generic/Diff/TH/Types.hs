{-#  LANGUAGE TemplateHaskell, DeriveDataTypeable #-}
module Data.Generic.Diff.TH.Types where
--import Control.Lens
import Data.Data
import Language.Haskell.TH

data FamConType = Concrete
                | Abstract
            deriving(Data, Typeable, Show, Eq)

data FamCon = FamCon {
        _famConHardness     :: FamConType,
        _famConName         :: Name      ,
        _famConOriginalName :: Name      ,
        _famConTypes        :: [Type]
    }
    deriving(Data, Typeable, Show, Eq)

--makeLenses ''FamCon

data FamType = FamType {
        _famTypeType         :: Type    ,
        _famTypeConstructors :: [FamCon]
    }
    deriving(Data, Typeable, Show, Eq)

--makeLenses ''FamType

data Fam = Fam {
        _famName  :: Name     ,
        _famTypes :: [FamType]
    }
    deriving(Data, Typeable, Show, Eq)

--makeLenses ''Fam
