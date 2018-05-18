{-# LANGUAGE FlexibleContexts #-}

module Graphics.Utils
  ( module Data.Colour.Names
  , Pos
  , Color
  , Block(..)
  , Blocks(..)
  , HasPos(..)
  , HasColor(..)
  , usa
  , parseColor
  , getRandomPos
  , getNRandomPos) where

import Data.Word
import Foreign.C.Types

import Control.Lens
import Control.Monad.Except
import Control.Monad.Random
import Data.Colour.Names
import Data.Colour.SRGB
import qualified Data.Set as S
import qualified Data.Vector as V
import SDL

type Pos = V2 CInt
type Color = V4 Word8

data Block = Block Pos Color
           deriving (Eq)

class HasPos a where
  pos :: Lens' a Pos

instance HasPos Block where
  pos = lens (\b -> case b of Block p _ -> p) (\b p -> case b of Block _ c -> Block p c)

class HasColor a where
  color :: Lens' a Color

instance HasColor Block where
  color = lens (\b -> case b of Block _ c -> c) (\b c -> case b of Block p _ -> Block p c)

class Blocks a where
  blocks :: Traversal' a Block

usa :: (Ord a, RealFrac a, Floating a) => Colour a -> Color
usa c = case toSRGB24 c of RGB r g b -> V4 r g b 0

parseColor :: (MonadError String m) => String -> m Color
parseColor str = maybe (throwError ("couldn't read color " ++ str)) (return . usa) (readColourName str)

getNRandomPos :: (MonadRandom m) => Int -> Pos -> S.Set Pos -> m (V.Vector Pos)
getNRandomPos 0 _ _  = return V.empty
getNRandomPos n b ps = do
  newPos <- getRandomPos b ps
  (V.cons newPos) <$> getNRandomPos (n-1) b (S.insert newPos ps)

getRandomPos :: (MonadRandom m) => Pos -> S.Set Pos -> m Pos
getRandomPos bounds@(V2 x y) ps = do
  newX <- getRandomR (0,x-1)
  newY <- getRandomR (0,y-1)
  let newPos = V2 newX newY
  if S.member newPos ps
    then getRandomPos bounds ps
    else return newPos

