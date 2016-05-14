module Neural (NeuralNetwork, buildNeuralNetwork, calculateOutputValues) where

import Data.List
import Data.List.Split

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

calculateOutputValues :: [Value] -> NeuralNetwork -> [Value]
calculateOutputValues inputValues (NeuralNetwork verticeGroups) = foldl' calculateLayerValues inputValues verticeGroups

calculateLayerValues :: [Value] -> [[Value]] -> [Value]
calculateLayerValues previousLayer verticeGroup = map (calculateNode previousLayer) verticeGroup
    where calculateNode previousLayer weights = squash $ sum $ zipWith (*) previousLayer weights
              where squash x = 1 / (1 + ((exp 1) ** (negate x)))
