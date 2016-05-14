module Neural (NeuralNetwork, buildNeuralNetwork, buildRandomNeuralNetwork, calculateOutputValues) where

import Data.List
import Data.List.Split
import System.Random

type Value = Double

data NeuralNetwork = NeuralNetwork [[[Value]]] deriving Show -- weight values grouped by vertice group, terminus, origin.

buildStaticNeuralNetwork :: [Int] -> NeuralNetwork
buildStaticNeuralNetwork nodeLayerSizes = buildNeuralNetwork nodeLayerSizes []

buildNeuralNetwork :: [Int] -> [Value] -> NeuralNetwork
buildNeuralNetwork nodeLayerSizes weights
    | numWeights < numVertices = buildNeuralNetwork nodeLayerSizes padWeights
    | otherwise = NeuralNetwork verticeGroups
    where verticeGroups = map (\(vtg,numTermini) -> chunksOf numTermini vtg) (zip groupWeightsByVerticeGroup terminusLayerSizes)
          groupWeightsByVerticeGroup = splitPlaces numVerticesByLayer weights
          numWeights = length weights
          numVertices = sum numVerticesByLayer
          numVerticesByLayer = zipWith (*) nodeLayerSizes terminusLayerSizes
          terminusLayerSizes = drop 1 nodeLayerSizes
          padWeights = weights ++ (take (numVertices - numWeights) $ repeat 0.0)

buildRandomNeuralNetwork :: [Int] -> IO NeuralNetwork
buildRandomNeuralNetwork nodeLayerSizes = do
    g <- newStdGen
    let randomVerticeValues = take numVertices $ randomRs (-8.0,8.0) g
            where numVertices = sum $ zipWith (*) nodeLayerSizes (drop 1 nodeLayerSizes)
    return $ buildNeuralNetwork nodeLayerSizes randomVerticeValues

calculateOutputValues :: [Value] -> NeuralNetwork -> [Value]
calculateOutputValues inputValues (NeuralNetwork verticeGroups) = foldl' calculateLayerValues inputValues verticeGroups

calculateLayerValues :: [Value] -> [[Value]] -> [Value]
calculateLayerValues previousLayer verticeGroup = map (calculateNode previousLayer) verticeGroup
    where calculateNode previousLayer weights = squash $ sum $ zipWith (*) previousLayer weights
              where squash x = 1 / (1 + ((exp 1) ** (negate x)))
