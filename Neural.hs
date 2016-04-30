module Neural where

import Data.Word

type Name = String
type Value = Double

data Vertice = Vertice Name Value Name

data NeuralNetwork = NeuralNetwork [Name] [Vertice] [Name] [Vertice] [Name]

makeNeuralNetworkWithHiddenNodes :: Int -> NeuralNetwork
makeNeuralNetworkWithHiddenNodes num = NeuralNetwork [] [] ["H" ++ show x | x <- [0..num]] [] []

addInput :: NeuralNetwork -> Name -> NeuralNetwork
addInput (NeuralNetwork inputs firstVerticeGroup hiddens secondVerticeGroup outputs) input = NeuralNetwork (input:inputs) ((generateStaticVerticesForInput input hiddens) ++ firstVerticeGroup) hiddens secondVerticeGroup outputs

generateStaticVerticesForInput :: Name -> [Name] -> [Vertice]
generateStaticVerticesForInput origin termini = map (\terminus -> Vertice origin 0.1 terminus) termini

addOutput :: NeuralNetwork -> Name -> NeuralNetwork
addOutput (NeuralNetwork inputs firstVerticeGroup hiddens secondVerticeGroup outputs) output = NeuralNetwork inputs firstVerticeGroup hiddens ((generateStaticVerticesForOutput output hiddens) ++ secondVerticeGroup) (output:outputs)

generateStaticVerticesForOutput :: Name -> [Name] -> [Vertice]
generateStaticVerticesForOutput terminus origins = map (\origin -> Vertice origin 0.1 terminus) origins
