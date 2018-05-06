{-# LANGUAGE FlexibleContexts #-}

module Graphics.Display (drawGame, getWindowSize) where

import Data.Game
import Graphics.Utils

import Control.Lens hiding (_head, _tail)
import Control.Monad.Reader
import Control.Monad.State
import qualified Data.Map as M
import SDL

getWindowSize :: Game -> Pos
getWindowSize g = ((g^.board) + 2) * (g^.gridSize)

drawGame :: (MonadReader Renderer m, MonadState Game m, MonadIO m) => m ()
drawGame = do
  render <- ask
  rendererDrawColor render $= (usa black)
  clear render
  drawBoard
  ps <- use players
  os <- use objects
  mapM_ (drawBlock (pure 4)) $ M.mapWithKey toBlock os
  mapM_ (drawBlock (pure 2)) (ps^..traverse.snake.body._tail.traverse)
  mapM_ (drawBlock (pure 2)) (ps^..traverse.snake.body._head)
  present render

drawBoard :: (MonadReader Renderer m, MonadState Game m, MonadIO m) => m ()
drawBoard = do
  render <- ask
  bSize <- use board
  gSize <- use gridSize
  bColor <- use background
  rendererDrawColor render $= bColor
  fillRect render $ Just (Rectangle (P gSize) (bSize * gSize))

drawBlock :: (MonadReader Renderer m, MonadState Game m, MonadIO m) => Pos -> Block -> m ()
drawBlock s (Block p c) = do
  render <- ask
  gSize <- use gridSize
  rendererDrawColor render $= c
  fillRect render $ Just (Rectangle (P (((p + pure 1) * gSize) + s)) (gSize - (2 * s)))
