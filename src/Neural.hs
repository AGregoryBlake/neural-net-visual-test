module Neural (NeuralNetwork, buildNeuralNetwork, buildRandomNeuralNetwork, calculateOutputValues, mutate) where

import Data.List
import Data.List.Split
import System.Random

type Value = Double

data NeuralNetwork = NeuralNetwork Value [Int] [[[Value]]] deriving Show -- weight values grouped by vertice group, terminus, origin.

buildStaticNeuralNetwork :: Value -> [Int] -> NeuralNetwork
buildStaticNeuralNetwork mutationRate nodeLayerSizes = buildNeuralNetwork mutationRate nodeLayerSizes []

buildNeuralNetwork :: Value -> [Int] -> [Value] -> NeuralNetwork
buildNeuralNetwork mutationRate nodeLayerSizes weights
    | numWeights < numVertices = buildNeuralNetwork mutationRate nodeLayerSizes padWeights
    | otherwise = NeuralNetwork mutationRate nodeLayerSizes verticeGroups
    where verticeGroups = map (\(vtg,numOrigins) -> chunksOf numOrigins vtg) (zip groupWeightsByVerticeGroup originLayerSizes)
          groupWeightsByVerticeGroup = splitPlaces numVerticesByLayer weights
          numWeights = length weights
          numVertices = sum numVerticesByLayer
          numVerticesByLayer = zipWith (*) nodeLayerSizes terminusLayerSizes
          terminusLayerSizes = drop 1 nodeLayerSizes
          originLayerSizes = init nodeLayerSizes
          padWeights = weights ++ (take (numVertices - numWeights) $ repeat 0.0)

buildRandomNeuralNetwork :: (Value,Value) -> [Int] -> IO NeuralNetwork
buildRandomNeuralNetwork verticeRange nodeLayerSizes = do
    g <- newStdGen
    let (mutationRate,g') = randomR (0.0,1.0) g
        randomVerticeValues = take numVertices $ randomRs verticeRange g'
            where numVertices = sum $ zipWith (*) nodeLayerSizes (drop 1 nodeLayerSizes)
    return $ buildNeuralNetwork mutationRate nodeLayerSizes randomVerticeValues

calculateOutputValues :: [Value] -> NeuralNetwork -> [Value]
calculateOutputValues inputValues (NeuralNetwork _ _ verticeGroups) = foldl' calculateLayerValues inputValues verticeGroups

calculateLayerValues :: [Value] -> [[Value]] -> [Value]
calculateLayerValues previousLayer verticeGroup = map (calculateNode previousLayer) verticeGroup
    where calculateNode previousLayer weights = squash $ sum $ zipWith (*) previousLayer weights
              where squash x = 1 / (1 + ((exp 1) ** (negate x)))

mutate :: (Value,Value) -> NeuralNetwork -> IO NeuralNetwork
mutate verticeRange neural@(NeuralNetwork mutationRate nodeLayerSizes verticeGroups) = do
    g <- newStdGen
    let (chance,g') = randomR (0.0,1.0) g
    if chance <= mutationRate then mutateVertices g' else return neural
        where mutateVertices g = do
                  return $ buildNeuralNetwork mutationRate nodeLayerSizes newValues
                  where values = concat $ concat verticeGroups
                        (verticeToReplace,g') = randomR (0,(length values) - 1) g
                        (left,(_:right)) = splitAt verticeToReplace values
                        (newValue,_) = randomR verticeRange g'
                        newValues = left ++ [newValue] ++ right

