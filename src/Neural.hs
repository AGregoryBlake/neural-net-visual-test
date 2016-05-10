module Neural (NeuralNetwork, buildNeuralNetwork, calculateOutputValues) where

import Data.List

type Value = Double

data NeuralNetwork = NeuralNetwork Int [[Value]] Int [[Value]] Int -- Weight values are sorted by terminus, then by origin.

buildStaticNeuralNetwork :: Int -> Int -> Int -> NeuralNetwork
buildStaticNeuralNetwork numInputs numHiddens numOutputs = buildNeuralNetwork numInputs numHiddens numOutputs []

buildNeuralNetwork :: Int -> Int -> Int -> [Value] -> NeuralNetwork
buildNeuralNetwork numInputs numHiddens numOutputs weights
    | numWeights < numVertices = buildNeuralNetwork numInputs numHiddens numOutputs padWeights
    | otherwise = NeuralNetwork numInputs firstVerticeGroup numHiddens secondVerticeGroup numOutputs
    where firstVerticeGroup = chunk numHiddens vtg1
          secondVerticeGroup = chunk numOutputs vtg2
          chunk n = takeWhile (not.null) . unfoldr (Just . splitAt n)
          (vtg1,vtg2) = splitAt numVerticesInFirstLayer weights
              where numVerticesInFirstLayer = numInputs * numHiddens
          numWeights = length weights
          numVertices = (numInputs + numOutputs) * numHiddens
          padWeights = weights ++ (take (numVertices - numWeights) $ repeat 0.0)

calculateOutputValues :: [Value] -> NeuralNetwork -> [Value]
calculateOutputValues inputValues (NeuralNetwork numInputs firstVerticeGroup numHiddens secondVerticeGroup numOutputs) = calculateOutputValuesInternal inputValues firstVerticeGroup secondVerticeGroup

calculateOutputValuesInternal :: [Value] -> [[Value]] -> [[Value]] -> [Value]
calculateOutputValuesInternal inputValues firstVerticeGroup secondVerticeGroup = calculateLayerValues hiddenValues secondVerticeGroup
    where hiddenValues = calculateLayerValues inputValues firstVerticeGroup

calculateLayerValues :: [Value] -> [[Value]] -> [Value]
calculateLayerValues previousLayer verticeGroup = map (calculateNode previousLayer) verticeGroup
    where calculateNode previousLayer weights = squash $ sum $ zipWith (*) previousLayer weights
              where squash x = 1 / (1 + ((exp 1) ** (negate x)))
