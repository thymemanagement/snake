{-# LANGUAGE FlexibleContexts #-}

module Control.Update (runGame) where

import Control.Input
import Data.Game
import Graphics.Display

import Control.Monad

import Control.Lens
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Random
import Data.Time
import SDL

runGame :: (MonadReader Renderer m, MonadState Game m, MonadIO m, MonadRandom m) => m ()
runGame = do
    runInput
    ct <- liftIO getCurrentTime
    lt <- use lastUpdate
    r  <- use rate
    when (diffUTCTime ct lt > r) $ do 
      updateGame
      drawGame
      lastUpdate .= ct
    runGame
