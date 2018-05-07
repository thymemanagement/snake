{-# LANGUAGE TemplateHaskell #-}

module Data.GameObj
  ( AppleObj(..)
  , GameObj(..)
  , nutrition
  , _Apple
  , _Wall
  , newApple
  , newWall
  , toBlock) where

import Graphics.Utils

import Control.Lens
import SDL

data AppleObj = AppleObj Color Int

data WallObj = WallObj Color
                
data GameObj = Apple AppleObj | Wall WallObj

makePrisms ''GameObj

instance HasColor AppleObj where
  color = lens (\a -> case a of AppleObj c _ -> c) (\a c -> case a of AppleObj _ i -> AppleObj c i)

instance HasColor WallObj where
  color = lens (\w -> case w of WallObj c -> c) (\w c -> case w of WallObj _ -> WallObj c)

instance HasColor GameObj where
  color = lens (\o -> case o of Apple a -> a^.color; Wall w -> w^.color) (\o c -> o & _Apple.color .~ c & _Wall.color .~ c)

nutrition :: Lens' AppleObj Int
nutrition = lens (\a -> case a of AppleObj _ i -> i) (\a i -> case a of AppleObj c _ -> AppleObj c i)

newApple :: Color -> Int -> GameObj
newApple c = Apple . AppleObj c

newWall :: Color -> GameObj
newWall = Wall . WallObj

toBlock :: Pos -> GameObj -> Block
toBlock p o = Block p (o^.color)


