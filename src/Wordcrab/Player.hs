{-# LANGUAGE TemplateHaskell #-}
module Wordcrab.Player where

import Control.Lens (makeLenses)

import qualified Wordcrab.Tiles as Tiles

data Player = Player
  { _score :: Integer
  , _rack :: [Tiles.Tile]
  } deriving Eq

makeLenses ''Player
