module Neural where

import Data.Word
       
type Squash = (Double -> Word8)
     
data NeuralNetwork = NeuralNetwork { inputs :: [Double]
                                   , weights :: [(Double -> Double)]
                                   }
