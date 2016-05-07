module Neural where

import Data.Maybe

type Name = String
type Value = Double

data Vertice = Vertice Name Value Name

data NeuralNetwork = NeuralNetwork [Name] [Vertice] [Name] [Vertice] [Name]

-- new in v0.2
getInputs :: NeuralNetwork -> [Name]
getInputs (NeuralNetwork inputs _ _ _ _) = inputs

-- new in v0.2
getOutputs :: NeuralNetwork -> [Name]
getOutputs (NeuralNetwork _ _ _ _ outputs) = outputs

-- new in v0.2
buildStaticNeuralNetworkInOneGo :: [Name] -> [Name] -> [Name] -> NeuralNetwork
buildStaticNeuralNetworkInOneGo inputs hiddens outputs = buildNeuralNetworkInOneGo inputs hiddens outputs []

-- new in v0.2
buildNeuralNetworkInOneGo :: [Name] -> [Name] -> [Name] -> [Value] -> NeuralNetwork
buildNeuralNetworkInOneGo inputs hiddens outputs weights
    | numWeights < numVertices = buildNeuralNetworkInOneGo inputs hiddens outputs padWeights
    | otherwise = NeuralNetwork inputs firstVerticeGroup hiddens secondVerticeGroup outputs
    where firstVerticeGroup = map tupleToVertice (addWeights inputHiddenPairs w1)
          secondVerticeGroup = map tupleToVertice (addWeights hiddenOutputPairs w2)
          (w1,w2) = splitAt numVerticesInFirstLayer weights
              where numVerticesInFirstLayer = (length inputs) * (length hiddens)
          tupleToVertice (o,w,t) = Vertice o w t
          addWeights nodePairs weights = zipWith (\(o,t) w -> (o,w,t)) nodePairs weights
          inputHiddenPairs = (,) <$> inputs <*> hiddens
          hiddenOutputPairs = (,) <$> hiddens <*> outputs
          numWeights = length weights
          numVertices = ((length inputs) + (length outputs)) * (length hiddens)
          padWeights = weights ++ (take (numVertices - numWeights) $ repeat 0.0)

calculateOutputValues :: [Name] -> [(Name,Value)] -> [Vertice] -> [Name] -> [Vertice] -> [(Name,Value)]
calculateOutputValues nodeNames inputs firstVerticeGroup hiddenNames secondVerticeGroup = map (\nodeName -> calculateOutputValue nodeName inputs firstVerticeGroup hiddenNames secondVerticeGroup) nodeNames

calculateOutputValue :: Name -> [(Name,Value)] -> [Vertice] -> [Name] -> [Vertice] -> (Name,Value)
calculateOutputValue nodeName inputs firstVerticeGroup hiddenNames secondVerticeGroup = calculateNodeValue nodeName hiddenNodeValues secondVerticeGroup
    where hiddenNodeValues = calculateNodeValues hiddenNames inputs firstVerticeGroup

calculateNodeValues :: [Name] -> [(Name,Value)] -> [Vertice] -> [(Name,Value)]
calculateNodeValues nodeNames inputs verticeGroup = map (\nodeName -> calculateNodeValue nodeName inputs verticeGroup) nodeNames

calculateNodeValue :: Name -> [(Name,Value)] -> [Vertice] -> (Name,Value)
calculateNodeValue nodeName inputs verticeGroup = (nodeName, nodeValue)
    where nodeValue = squash $ sum $ verticeNodeValues
          verticeNodeValues = map calculateVerticeNodeValue verticeValuePairs
          verticeValuePairs = map (\vertice -> matchVerticeWithOriginValue vertice inputs) filteredVertices
          filteredVertices = getVerticesWithTerminusNamed nodeName verticeGroup

squash :: Value -> Value
squash x = 1 / (1 + (e ** (negate x)))
    where e = exp 1

calculateVerticeNodeValue :: (Vertice,Value) -> Value
calculateVerticeNodeValue ((Vertice _ verticeValue _),nodeValue) = verticeValue * nodeValue

matchVerticeWithOriginValue :: Vertice -> [(Name,Value)] -> (Vertice,Value)
matchVerticeWithOriginValue vertice@(Vertice name _ _) inputs = (vertice,originValue)
    where originValue = if isNothing (lookup name inputs) then 0.0 else fromJust (lookup name inputs)

getVerticesWithOriginNamed :: Name -> [Vertice] -> [Vertice]
getVerticesWithOriginNamed originName vertices = filter (\(Vertice origin _ _) -> originName == origin) vertices

getVerticesWithTerminusNamed :: Name -> [Vertice] -> [Vertice]
getVerticesWithTerminusNamed terminusName vertices = filter (\(Vertice _ _ terminus) -> terminusName == terminus) vertices

getSpecificOutputValue :: Name -> [(Name,Value)] -> Value
getSpecificOutputValue name outputs = if isNothing (lookup name outputs) then 0.0 else fromJust (lookup name outputs)
