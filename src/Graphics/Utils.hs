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
  , readColor) where

import Data.Word
import Foreign.C.Types

import Control.Lens
import Control.Monad.Except
import Data.Colour.Names
import Data.Colour.SRGB
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

readColor :: (MonadError String m) => String -> m Color
readColor str = maybe (throwError ("couldn't read color " ++ str)) (return . usa) (readColourName str)

