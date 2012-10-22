module Old where

data Song = Song {
        instruments :: [Instruments]
    }

data Instrument = Instrument {
        notes :: [Note]
    }
    
type Note = (Double, Model)

data Model = Exp Model Model
           | ADSR Model Model Model Model
           | Base Double