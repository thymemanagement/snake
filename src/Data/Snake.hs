{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}

module Data.Snake
  ( Snake(Snake)
  , HasHead(..)
  , HasTail(..)
  , HasLife(..)
  , curDir
  , lastDir
  , newSnake
  , updateSnake
  , addSnake
  , killSnake) where

import Graphics.Utils

import Prelude hiding (head, tail, init)
import Data.Function
import Data.List.NonEmpty hiding (length)

import Control.Lens hiding (_head, _tail, (<|))

data Snake = Snake { _body :: NonEmpty Block,
                     _curDir :: Pos,
                     _lastDir :: Pos,
                     _size :: Int,
                     _isDead :: Bool }

makeLenses ''Snake

class HasHead s where
  _head :: Lens' s Block

instance HasHead (NonEmpty Block) where
  _head = lens head (\xs x -> x :| tail xs)

instance HasHead Snake where
  _head = body.(lens head (\xs x -> x :| tail xs))

class HasTail s where
  _tail :: Traversal' s Block

instance HasTail (NonEmpty Block) where
  _tail = (lens tail (\xs t -> head xs :| t)) . traverse

instance HasTail Snake where
  _tail = body._tail

class HasLife a where
  dead :: Prism' a a
  alive :: Prism' a a

instance HasLife Snake where
  dead = prism' id (\s -> if s^.isDead then Just s else Nothing)
  alive = prism' id (\s -> if s^.isDead then Nothing else Just s)

instance HasPos Snake where
  pos = _head.pos

instance HasColor Snake where
  color = _head.color

instance Blocks Snake where
  blocks = body.traverse

newSnake :: Block -> Snake
newSnake b = Snake (b :| []) (pure 0) (pure 0) 1 False

moveSnake :: Snake -> Snake
moveSnake s = s & body %~ (\xs -> (head xs & pos +~ (s^.curDir)) :| init xs) & lastDir .~ (s^.curDir)

growSnake :: Snake -> Snake
growSnake s = s & body %~ (\xs -> (head xs & pos +~ (s^.curDir)) <| xs) & lastDir .~ (s^.curDir)

updateSnake :: Snake -> Snake
updateSnake s
  | snakeIdle s                  = s
  | length (s^.body) < (s^.size) = growSnake s
  | otherwise                    = moveSnake s

addSnake :: Int -> Snake -> Snake
addSnake n s = s & size +~ n

killSnake :: Snake -> Snake
killSnake s = s & isDead .~ True & _head.color .~ (usa red) & _tail.color .~ (usa salmon)

snakeIdle :: Snake -> Bool
snakeIdle s = s^.curDir == 0
