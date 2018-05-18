{-# LANGUAGE TemplateHaskell #-}

module Data.Player
  ( module Data.Snake
  , Player(..)
  , snake
  , bindings
  , base
  , newPlayer
  , updatePlayer) where

import Data.Snake
import Graphics.Utils

import Data.Function

import Control.Lens hiding (_head, _tail)
import qualified Data.Map as M
import SDL

data Player = Player { _snake :: Snake,
                 _bindings :: M.Map Scancode (Player -> Player),
                 _base :: Block }

makeLenses ''Player

class HasPlayers a where
  player :: Traversal' a Player

instance HasPlayers Player where
  player = id

instance HasHead Player where
  _head = snake._head

instance HasTail Player where
  _tail = snake._tail

instance HasLife Player where
  dead = filtered (view (snake.isDead))
  alive = filtered (not . view (snake.isDead))

instance HasPos Player where
  pos = snake.pos

instance HasColor Player where
  color = snake.color

instance Blocks Player where
  blocks = snake.blocks

newPlayer :: Block -> Scancode -> Scancode -> Scancode -> Scancode -> Scancode -> Player
newPlayer b u d l r n = Player (newSnake b) (makeMoveset u d l r n) b

makeMoveset :: Scancode -> Scancode -> Scancode -> Scancode -> Scancode -> M.Map Scancode (Player -> Player)
makeMoveset u d l r n = M.fromList [(u, moveUp), (d, moveDown), (l, moveLeft), (r, moveRight), (n, spawnSnake)]

updatePlayer :: Player -> Player
updatePlayer = snake.alive %~ updateSnake

spawnSnake :: Player -> Player
spawnSnake p = p & snake.dead .~ newSnake (p^.base)

moveDir :: Pos -> Player -> Player
moveDir d = snake.alive %~ moveDir'
  where moveDir' s = if (s^.._tail) == [] || crossZ (s^.lastDir) d /= 0
                     then s & curDir .~ d
                     else s

moveLeft :: Player -> Player
moveLeft = moveDir $ V2 (-1) 0

moveRight :: Player -> Player
moveRight = moveDir $ V2 1 0

moveUp :: Player -> Player
moveUp = moveDir $ V2 0 (-1)

moveDown :: Player -> Player
moveDown = moveDir $ V2 0 1
