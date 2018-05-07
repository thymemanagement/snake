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
  , gridSize
  , rate
  , lastUpdate
  , updateGame
  , getRandomPos
  , getNRandomPos) where

import Data.GameObj
import Data.Player
import Graphics.Utils

import Control.Lens hiding (_head, _tail)
import Control.Monad.State
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Set.Lens
import Data.Time
import qualified Data.Vector as V
import SDL
import System.Random


type ObjectMap = M.Map Pos GameObj

data Game = Game { _players :: V.Vector Player
                 , _objects :: ObjectMap
                 , _board :: Pos
                 , _gridSize :: Pos
                 , _background :: Color
                 , _rate :: NominalDiffTime
                 , _lastUpdate :: UTCTime }

makeLenses ''Game

updateGame :: (MonadState Game m, MonadIO m) => m ()
updateGame = do
    players.traverse %= updatePlayer
    checkCollisions

checkCollisions :: (MonadState Game m, MonadIO m) => m ()
checkCollisions = do
  ps <- use players
  os <- use objects
  players <~ (traverse.alive) (snakeObjCollision os) ps
  ps' <- use players
  players <~ (traverse.alive) snakeSnakeCollision ps'

killPlayer :: (MonadState Game m, MonadIO m) => Player -> m Player
killPlayer p = do
  p' <- findNewSnakeLocation p
  return (p' & snake %~ killSnake)  

snakeSnakeCollision :: (MonadState Game m, MonadIO m) => Player -> m Player
snakeSnakeCollision p = do
  let curPos = p^.pos
  bSize <- use board
  collision <- snakeHitSnake curPos
  if posOutOfBounds bSize curPos || collision
    then killPlayer p
    else return p

posOutOfBounds :: Pos -> Pos -> Bool
posOutOfBounds (V2 w h) (V2 x y) = x < 0 || y < 0 || x >= w || y >= h

snakeHitSnake :: (MonadState Game m) => Pos -> m Bool
snakeHitSnake p = do
  ps <- use players
  return (case filter (==p) (ps^..traverse.blocks.pos) of _:[] -> False; _ -> True)

snakeObjCollision :: (MonadState Game m, MonadIO m) => ObjectMap -> Player -> m Player
snakeObjCollision os p = M.findWithDefault (return p) (p^.pos) $ M.mapWithKey (objCollision p) os

objCollision :: (MonadState Game m, MonadIO m) => Player -> Pos -> GameObj -> m Player
objCollision p v (Apple a) = appleCollision p v a
objCollision p _ (Wall _)     = killPlayer p

appleCollision :: (MonadState Game m, MonadIO m) => Player -> Pos -> AppleObj -> m Player
appleCollision p oldPos a = do
  findNewObjLocation oldPos
  return (p & snake %~ addSnake (a^.nutrition))

findNewObjLocation :: (MonadState Game m, MonadIO m) => Pos -> m ()
findNewObjLocation oldPos = do
  range <- use board
  objSet <- fmap M.keysSet (use objects)
  snakeSet <- fmap (setOf (traverse.alive.blocks.pos)) (use players)
  newPos <- getRandomPos range (objSet <> snakeSet)
  object <- use (objects.at oldPos)
  objects.at oldPos .= Nothing
  objects.at newPos .= object


findNewSnakeLocation :: (MonadState Game m, MonadIO m) => Player -> m Player
findNewSnakeLocation p = do
  range <- use board
  objSet <- M.keysSet <$> use objects
  snakeSet <- setOf (traverse.alive.blocks.pos) <$> use players
  newPos <- getRandomPos range (objSet <> snakeSet S.\\ (setOf (_tail.pos) p))
  return (p & base.pos .~ newPos)

getNRandomPos :: (MonadIO m) => Int -> Pos -> S.Set Pos -> m (V.Vector Pos)
getNRandomPos 0 _ _  = return V.empty
getNRandomPos n b ps = do
  newPos <- getRandomPos b ps
  (V.cons newPos) <$> getNRandomPos (n-1) b (S.insert newPos ps)

getRandomPos :: (MonadIO m) => Pos -> S.Set Pos -> m Pos
getRandomPos bounds@(V2 x y) ps = do
  newX <- liftIO $ randomRIO (0,x-1)
  newY <- liftIO $ randomRIO (0,y-1)
  let newPos = V2 newX newY
  if S.member newPos ps
    then getRandomPos bounds ps
    else return newPos
