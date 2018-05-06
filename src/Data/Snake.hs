{-# LANGUAGE TemplateHaskell #-}

module Data.Snake
  ( Snake(..)
  , pos
  , style
  , body
  , curDir
  , lastDir
  , size
  , dead
  , alive
  , _head
  , _tail
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
                     __dead :: Bool }

makeLenses ''Snake

dead :: Prism' Snake Snake
dead = prism' id (\s -> if s^._dead then Just s else Nothing)

alive :: Prism' Snake Snake
alive = prism' id (\s -> if s^._dead then Nothing else Just s)

_head :: Lens' (NonEmpty a) a
_head f xs = fmap (:| tail xs) . f . head $ xs

_tail :: Lens' (NonEmpty a) [a]
_tail f xs = fmap (head xs :|) . f . tail $ xs

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
killSnake s = s & _dead .~ True & body %~ \xs ->
  (head xs & style .~ (usa red)) :| (tail xs & traverse.style .~ (usa salmon))

snakeIdle :: Snake -> Bool
snakeIdle s = s^.curDir == 0
