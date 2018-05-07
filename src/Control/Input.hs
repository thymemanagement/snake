{-# LANGUAGE FlexibleContexts #-}

module Control.Input (runInput) where

import Data.Game

import System.Exit

import Control.Lens
import Control.Monad.State
import Data.Map
import SDL

runInput :: (MonadState Game m, MonadIO m) => m ()
runInput = mapEvents handleEvent

handleEvent :: (MonadState Game m, MonadIO m) => Event -> m ()
handleEvent event = case eventPayload event of
  KeyboardEvent kData -> if keyboardEventKeyMotion kData == Pressed
                         then let sCode = keysymScancode . keyboardEventKeysym $ kData
                              in players.traverse %= \p -> findWithDefault id sCode (p^.bindings) p
                         else return ()
  WindowClosedEvent _ -> liftIO exitSuccess
  _ -> return ()
