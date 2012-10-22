module New where

data Song = Song {
        instruments :: [Instruments],
        name        :: String
    }
    deriving(Show, Eq)

data Instrument = Instrument {
        notes :: [Note],
        typ   :: InstrumentType
    }
    deriving(Show, Eq)
    
newtype Note = Note { unNote :: (Double, Model) }
    deriving(Show, Eq)

data Model = Exp Model Model
           | ADSR Model Model Model Model
           | Base Double
               deriving(Show, Eq)