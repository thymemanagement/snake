{-# LANGUAGE OverloadedStrings #-}
-- | A stupid little program that prints out keyboard scancodes if you type in the window.

module Main where

import Control.Monad
import System.Exit

import SDL

main :: IO ()
main = do
  initializeAll
  createWindow "What a stupid window" defaultWindow
  loopInput

loopInput :: IO ()
loopInput = do
  mapEvents handleEvent
  loopInput
  where handleEvent event = case eventPayload event of
          KeyboardEvent kData -> if keyboardEventKeyMotion kData == Pressed
                                 then do let sCode = keysymScancode . keyboardEventKeysym $ kData
                                         putStrLn . show . unwrapScancode $ sCode
                                         when (sCode == ScancodeEscape) exitSuccess
                                 else return ()
          WindowClosedEvent _ -> exitSuccess
          _ -> return ()
