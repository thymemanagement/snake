{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}

module Graphics.Utils
  ( module Data.Colour.Names
  , Pos
  , Color
  , Block(..)
  , pos
  , style
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

data Block = Block { _pos :: Pos,
                     _style :: Color }
           deriving (Eq)

makeLenses ''Block

usa :: (Ord a, RealFrac a, Floating a) => Colour a -> Color
usa c = case toSRGB24 c of RGB r g b -> V4 r g b 0

readColor :: (MonadError String m) => String -> m Color
readColor str = maybe (throwError ("couldn't read color " ++ str)) (return . usa) (readColourName str)

