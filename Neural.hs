module Neural where

import Data.Word

type Name = String
type Value = Double
type Terminus = Node

data Node = Node Name Value [Vertice]
data Vertice = Vertice Value Terminus

data NeuralNetwork = NeuralNetwork [Node]

createNewNetwork :: Name -> Value -> NeuralNetwork
createNewNetwork name value = NeuralNetwork [Node name value []]

addInput :: NeuralNetwork -> Name -> Value -> NeuralNetwork
addInput (NeuralNetwork xs@(x:_)) name value = NeuralNetwork (Node name value (getVertices x):xs)

getVertices :: Node -> [Vertice]
getVertices (Node _ _ vertices) = vertices

addHidden :: NeuralNetwork -> Name -> NeuralNetwork
addHidden (NeuralNetwork nodes) hiddenNodeName =
    let addHiddenToOne (Node inputName inputValue vertices) hiddenNodeName = Node inputName inputValue ((Vertice verticeValue (Node hiddenNodeName hiddenNodeValue [])):vertices)
        verticeValue = 0.1
        hiddenNodeValue = 5
    in NeuralNetwork (map (\node -> addHiddenToOne node hiddenNodeName) nodes)


-- addOutput :: NeuralNetwork -> Node -> NeuralNetwork
