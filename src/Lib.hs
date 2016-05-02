{-# LANGUAGE OverloadedStrings #-}
module Lib
    ( app
    ) where

import SDL
import Linear
import Linear.Affine
import Foreign.C.Types
import Control.Monad (unless)
import Data.Word
import Control.Concurrent
import Neural


windowWidth = 500 :: CInt
windowHeight = 500 :: CInt

neural = f
    where a = makeNeuralNetworkWithHiddenNodes 4
          b = addInput a "X"
          c = addInput b "Y"
          d = addOutput c "R"
          e = addOutput d "G"
          f = addOutput e "B"

app :: IO ()
app = appInit

appInit :: IO ()
appInit = do
    initializeAll
    window <- createWindow "Herbivores and Carnivores" defaultWindow
           { windowInitialSize = V2 windowWidth windowHeight }
    renderer <- createRenderer window (-1) defaultRenderer
    appLoop neural renderer

appLoop :: NeuralNetwork -> Renderer -> IO ()
appLoop neural renderer = do
        events <- pollEvents
        let eventIsKeyPress key event =
              case eventPayload event of
                KeyboardEvent keyboardEvent ->
                  keyboardEventKeyMotion keyboardEvent == Pressed &&
                  keysymKeycode (keyboardEventKeysym keyboardEvent) == key
                _ -> False
            keyPressed key = not (null (filter (eventIsKeyPress key) events))
            qPressed = keyPressed KeycodeQ
        rendererDrawColor renderer $= V4 0 0 0 255
        clear renderer
        drawScene neural renderer
        present renderer
        threadDelay 16500
        unless qPressed $ appLoop neural renderer

drawScene :: NeuralNetwork -> Renderer -> IO ()
drawScene neural renderer = do
    let generateAllPoints = [(P $ V2 (CInt (fromIntegral x)) (CInt (fromIntegral y))) | x <- [0..(fromIntegral windowWidth)] :: [Int], y <- [0..(fromIntegral windowHeight)]]
    allPoints <- return generateAllPoints
    mapM_ (\point -> drawNeuralOutputForPoint point neural renderer) allPoints

drawNeuralOutputForPoint :: Point V2 CInt -> NeuralNetwork -> Renderer -> IO ()
drawNeuralOutputForPoint point@(P (V2 (CInt x) (CInt y))) (NeuralNetwork inputNames firstVerticeGroup hiddenNames secondVerticeGroup outputNames) renderer = do
    let normalizedX = (fromIntegral x) / (fromIntegral windowWidth)
        normalizedY = (fromIntegral y) / (fromIntegral windowHeight)
        inputs = [("X",normalizedX),("Y",normalizedY)]
        getNormalizedOutputValue name outputValues = floor ((getSpecificOutputValue name outputValues) * 256)
    outputValues <- return (calculateOutputValues outputNames inputs firstVerticeGroup hiddenNames secondVerticeGroup)
    r <- return $ getNormalizedOutputValue "R" outputValues
    g <- return $ getNormalizedOutputValue "G" outputValues
    b <- return $ getNormalizedOutputValue "B" outputValues
    rendererDrawColor renderer $= V4 r g b 255
    drawPoint renderer point
