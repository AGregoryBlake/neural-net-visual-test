module Neural where

import Data.Word
import Data.Maybe

type Name = String
type Value = Double

data Vertice = Vertice Name Value Name

data NeuralNetwork = NeuralNetwork [Name] [Vertice] [Name] [Vertice] [Name]

makeNeuralNetworkWithHiddenNodes :: Int -> NeuralNetwork
makeNeuralNetworkWithHiddenNodes num = NeuralNetwork [] [] ["H" ++ show x | x <- [0..num]] [] []

addStaticInput :: NeuralNetwork -> Name -> NeuralNetwork
addStaticInput (NeuralNetwork inputs firstVerticeGroup hiddens secondVerticeGroup outputs) input = NeuralNetwork (input:inputs) ((generateStaticVerticesForInput input hiddens) ++ firstVerticeGroup) hiddens secondVerticeGroup outputs

addInput :: NeuralNetwork -> Name -> [Value] -> NeuralNetwork
addInput (NeuralNetwork inputs firstVerticeGroup hiddens secondVerticeGroup outputs) input values = NeuralNetwork (input:inputs) ((generateVerticesForInput input hiddens values) ++ firstVerticeGroup) hiddens secondVerticeGroup outputs

generateStaticVerticesForInput :: Name -> [Name] -> [Vertice]
generateStaticVerticesForInput origin termini = map (\terminus -> Vertice origin 2 terminus) termini

generateVerticesForInput :: Name -> [Name] -> [Double] -> [Vertice]
generateVerticesForInput origin termini terminiValues = map (\(t,tV) -> Vertice origin tV t) terminiValuePair
    where terminiValuePair = zip termini terminiValues

addStaticOutput :: NeuralNetwork -> Name -> NeuralNetwork
addStaticOutput (NeuralNetwork inputs firstVerticeGroup hiddens secondVerticeGroup outputs) output = NeuralNetwork inputs firstVerticeGroup hiddens ((generateStaticVerticesForOutput output hiddens) ++ secondVerticeGroup) (output:outputs)

addOutput :: NeuralNetwork -> Name -> [Value] -> NeuralNetwork
addOutput (NeuralNetwork inputs firstVerticeGroup hiddens secondVerticeGroup outputs) output values = NeuralNetwork inputs firstVerticeGroup hiddens ((generateVerticesForOutput output hiddens values) ++ secondVerticeGroup) (output:outputs)

generateStaticVerticesForOutput :: Name -> [Name] -> [Vertice]
generateStaticVerticesForOutput terminus origins = map (\origin -> Vertice origin 2 terminus) origins

generateVerticesForOutput :: Name -> [Name] -> [Double] -> [Vertice]
generateVerticesForOutput terminus origins originValues = map (\(o,oV) -> Vertice o oV terminus) originValuePair
    where originValuePair = zip origins originValues

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
