{-# LANGUAGE OverloadedStrings #-}
module Lib
    ( app
    ) where

import SDL
import Linear
import Linear.Affine
import Control.Monad (unless)
import Data.Word
import Control.Concurrent
import Neural
    
windowWidth = 500
windowHeight = 500
neural = [] []
    
app :: IO ()
app = appInit

appInit :: IO ()
appInit = do
    initializeAll
    window <- createWindow "Herbivores and Carnivores" defaultWindow
           { windowInitialSize = V2 windowWidth windowHeight }
    renderer <- createRenderer window (-1) defaultRenderer
    appLoop renderer neural

appLoop :: Renderer -> NeuralNetwork -> IO ()
appLoop renderer neural = do
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
        present renderer
        threadDelay 16500
        unless qPressed $ appLoop renderer neural
