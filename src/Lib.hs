{-# LANGUAGE OverloadedStrings #-}
module Lib (app) where

import SDL
import Linear
import Linear.Affine
import Foreign.C.Types
import Control.Monad (unless)
import Data.Word
import Control.Concurrent
import System.Random
import Neural


windowWidth = 250 :: CInt
windowHeight = 250 :: CInt

verticeRange = (-8.0,8.0)

app :: IO ()
app = appInit

appInit :: IO ()
appInit = do
    initializeAll
    window <- createWindow "Neural Network Test" defaultWindow
           { windowInitialSize = V2 windowWidth windowHeight }
    renderer <- createRenderer window (-1) defaultRenderer
    neural <- buildRandomNeuralNetwork verticeRange [2, 32, 16, 8, 3]
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
        neural' <- mutate verticeRange neural
        unless qPressed $ appLoop neural' renderer

drawScene :: NeuralNetwork -> Renderer -> IO ()
drawScene neural renderer = do
    let generateAllPoints = [(P $ V2 (CInt (fromIntegral x)) (CInt (fromIntegral y))) | x <- [0..(fromIntegral windowWidth)] :: [Int], y <- [0..(fromIntegral windowHeight)]]
    allPoints <- return generateAllPoints
    mapM_ (\point -> drawNeuralOutputForPoint point neural renderer) allPoints

drawNeuralOutputForPoint :: Point V2 CInt -> NeuralNetwork -> Renderer -> IO ()
drawNeuralOutputForPoint point@(P (V2 (CInt x) (CInt y))) neural renderer = do
    outputValues <- return (calculateOutputValues inputs neural)
    r <- getOutputValue 0 outputValues
    g <- getOutputValue 1 outputValues
    b <- getOutputValue 2 outputValues
    rendererDrawColor renderer $= V4 r g b 255
    drawPoint renderer point
        where inputs = [normalizedX, normalizedY]
              normalizedX = normalizedDim x windowWidth
              normalizedY = normalizedDim y windowHeight
              normalizedDim a b = (((fromIntegral a) - (0.5 * (fromIntegral b))) / (fromIntegral b))
              normalizeOutput a = floor (a * 256)
              getOutputValue n outputValues = return $ normalizeOutput $ outputValues !! n

