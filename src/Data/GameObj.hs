{-# LANGUAGE TemplateHaskell #-}

module Data.GameObj
  ( AppleObj(..)
  , GameObj(..)
  , hue
  , nutrition
  , _Apple
  , _Wall
  , newApple
  , toBlock) where

import Graphics.Utils

import Control.Lens
import SDL

data AppleObj = AppleObj { _hue :: Color
                         , _nutrition :: Int }
                
data GameObj = Apple AppleObj | Wall

makeLenses ''AppleObj
makePrisms ''GameObj

newApple :: Color -> Int -> GameObj
newApple c = Apple . AppleObj c

toBlock :: Pos -> GameObj -> Block
toBlock p (Apple a) = appleToBlock p a
toBlock v Wall  = Block v (usa grey)

appleToBlock :: Pos -> AppleObj -> Block
appleToBlock v (AppleObj c _) = Block v c


