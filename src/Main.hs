{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Update
import Data.Game.JSON
import Graphics.Display
import Paths_snake

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import SDL

main :: IO ()
main = do
  initializeAll
  jsonPath <- getDataFileName "res/config.json"
  game <- fmap (either error id) $ runExceptT (readGame jsonPath)
  window <- createWindow "Snake" defaultWindow
  windowSize window $= getWindowSize game
  renderer <- createRenderer window (-1) defaultRenderer
  runReaderT (evalStateT runGame game) renderer
