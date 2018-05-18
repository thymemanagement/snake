{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}

module Data.Game
  ( module Data.Player
  , module Data.GameObj
  , Game(..)
  , players
  , objects
  , board
  , background
  , edge
  , gridSize
  , rate
  , lastUpdate
  , updateGame
  , getRandomPos
  , getNRandomPos) where

import Data.GameObj
import Data.Player
import Graphics.Utils

import Data.List

import Control.Lens hiding (_head, _tail)
import Control.Monad.State
import Control.Monad.Random
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Set.Lens
import Data.Time
import qualified Data.Vector as V
import SDL


type ObjectMap = M.Map Pos GameObj

data Game = Game { _players :: V.Vector Player
                 , _objects :: ObjectMap
                 , _board :: Pos  --read only
                 , _gridSize :: Pos  --read only
                 , _background :: Color  --read only
                 , _edge :: Color  --read only
                 , _rate :: NominalDiffTime  --read only
                 , _lastUpdate :: UTCTime }

makeLenses ''Game

updateGame :: (MonadState Game m, MonadRandom m) => m ()
updateGame = do
    players.traverse %= updatePlayer
    checkCollisions

checkCollisions :: (MonadState Game m, MonadRandom m) => m ()
checkCollisions = do
  ps <- use players
  os <- use objects
  ps' <- (traverse.alive) (snakeObjCollision os) ps
  players <~ (traverse.alive) snakeSnakeCollision ps'

killPlayer :: (MonadState Game m, MonadRandom m) => Player -> m Player
killPlayer p = do
  p' <- findNewSnakeLocation p
  return (p' & snake %~ killSnake)  

snakeSnakeCollision :: (MonadState Game m, MonadRandom m) => Player -> m Player
snakeSnakeCollision p = do
  let curPos = p^.pos
  bSize <- use board
  collision <- gets (snakeHitSnake curPos)
  if posOutOfBounds bSize curPos || collision
    then killPlayer p
    else return p

posOutOfBounds :: Pos -> Pos -> Bool
posOutOfBounds (V2 w h) (V2 x y) = x < 0 || y < 0 || x >= w || y >= h

snakeHitSnake :: Pos -> Game -> Bool
snakeHitSnake p g = (elem p . delete p $ g^..players.traverse.alive._head.pos) || (elemOf (players.traverse.alive._tail.pos) p g)

snakeObjCollision :: (MonadState Game m, MonadRandom m) => ObjectMap -> Player -> m Player
snakeObjCollision os p = M.findWithDefault (return p) (p^.pos) $ M.mapWithKey (objCollision p) os

objCollision :: (MonadState Game m, MonadRandom m) => Player -> Pos -> GameObj -> m Player
objCollision p v (Apple a) = appleCollision p v a
objCollision p _ (Wall _)     = killPlayer p

appleCollision :: (MonadState Game m, MonadRandom m) => Player -> Pos -> AppleObj -> m Player
appleCollision p oldPos a = do
  findNewObjLocation oldPos
  return (p & snake %~ addSnake (a^.nutrition))

findNewObjLocation :: (MonadState Game m, MonadRandom m) => Pos -> m ()
findNewObjLocation oldPos = do
  range <- use board
  objSet <- fmap M.keysSet (use objects)
  snakeSet <- gets (setOf (players.traverse.alive.blocks.pos))
  newPos <- getRandomPos range (objSet <> snakeSet)
  object <- use (objects.at oldPos)
  objects.at oldPos .= Nothing
  objects.at newPos .= object


findNewSnakeLocation :: (MonadState Game m, MonadRandom m) => Player -> m Player
findNewSnakeLocation p = do
  range <- use board
  objSet <- M.keysSet <$> use objects
  snakeSet <- gets (setOf (players.traverse.alive.blocks.pos))
  newPos <- getRandomPos range (objSet <> snakeSet S.\\ (setOf (_tail.pos) p))
  return (p & base.pos .~ newPos)



  
