{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Game.JSON (readGame) where

import Data.Game
import Graphics.Utils

import Data.Foldable

import Control.Lens
import Control.Monad.Except
import Data.Time
import Data.Aeson
import Data.Aeson.Lens
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Set.Lens
import qualified Data.Text as T
import qualified Data.Vector as V
import SDL

readGame :: (MonadError String m, MonadIO m) => FilePath -> m Game
readGame path = do
  value <- liftEither =<< liftIO (eitherDecodeFileStrict path :: IO (Either String Value))
  rt <- testProperty "rate" . fmap (recip . realToFrac) $ (value^?_Object.ix "rate"._Double)
  bd <- readPos "width" "height" =<< (testProperty "board" $ (value^?_Object.ix "board"))
  gr <- readPos "width" "height" =<< (testProperty "gridSize" $ (value^?_Object.ix "gridSize"))
  bc <- readColor =<< (testProperty "background" . fmap T.unpack $ (value^?_Object.ix "background"._String))
  ps <- readPlayers bd =<< (testProperty "players" $ (value^?_Object.ix "players"._Array))
  os <- readObjects bd (setOf (traverse.snake.body.traverse.pos) ps) =<< (testProperty "objects" $ (value^?_Object.ix "objects"._Array))
  t <- liftIO getCurrentTime
  return (Game ps os bd gr bc rt t)

readPos :: (MonadError String m) => T.Text -> T.Text -> Value -> m Pos
readPos a b obj = do
  x <- testProperty a . fmap fromIntegral $ (obj^?_Object.ix a._Integer)
  y <- testProperty b . fmap fromIntegral $ (obj^?_Object.ix b._Integer)
  return (V2 x y)

readPlayers :: (MonadError String m, MonadIO m) => Pos -> V.Vector Value -> m (V.Vector Player)
readPlayers bounds arr = do
  rands <- getNRandomPos (V.length arr) bounds S.empty
  sequence . V.zipWith readPlayer rands $ arr

readPlayer :: (MonadError String m) => Pos -> Value -> m Player
readPlayer rand value = do
  c <- readColor =<< (testProperty "color" . fmap T.unpack $ (value^?_Object.ix "color"._String))
  p <- catchError (readPos "spawnX" "spawnY" value) (const (return rand)) 
  left <-  testProperty "left" . fmap (Scancode . fromIntegral) $ (value^?_Object.ix "left"._Integer)
  right <-  testProperty "right" . fmap (Scancode . fromIntegral) $ (value^?_Object.ix "right"._Integer)
  up <-  testProperty "up" . fmap (Scancode . fromIntegral) $ (value^?_Object.ix "up"._Integer)
  down <-  testProperty "down" . fmap (Scancode . fromIntegral) $ (value^?_Object.ix "down"._Integer)
  reset <-  testProperty "reset" . fmap (Scancode . fromIntegral) $ (value^?_Object.ix "reset"._Integer)
  return (newPlayer (Block p c) up down left right reset)

readObjects :: (MonadError String m, MonadIO m) => Pos -> S.Set Pos -> V.Vector Value -> m (M.Map Pos GameObj)
readObjects bounds ps arr = do
  os <- fmap fold . traverse readObject $ arr
  rands <- getNRandomPos (length os) bounds ps
  return (M.fromList . V.toList . V.zip rands $ os) 

readObject :: (MonadError String m) => Value -> m (V.Vector GameObj)
readObject value = do
  ct <- testProperty "count" . fmap fromIntegral $ (value^?_Object.ix "count"._Integer)
  ty <- testProperty "type" $ (value^?_Object.ix "type"._String)
  case ty of
    "apple" -> do
      cl <- readColor =<< (testProperty "color" . fmap T.unpack $ (value^?_Object.ix "color"._String))
      vl <- testProperty "value" . fmap fromIntegral $ (value^?_Object.ix "value"._Integer)
      return (V.replicate ct (newApple cl vl))
    "wall"  -> return (V.replicate ct Wall)
    xs      -> throwError ("Could not determine game object type of " ++ show xs)
  
failWith :: (MonadError e m) => e -> Maybe a -> m a
failWith e Nothing  = throwError e
failWith _ (Just x) = return x

testProperty :: (MonadError String m) => T.Text -> Maybe a -> m a
testProperty prop ma = failWith ("trouble reading property " ++ show prop) ma
