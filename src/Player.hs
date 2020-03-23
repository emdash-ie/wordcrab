{-# LANGUAGE TemplateHaskell #-}
module Player where

import Control.Lens (makeLenses)

import qualified Tiles

data Player = Player
  { _score :: Integer
  , _rack :: [Tiles.Tile]
  }

makeLenses ''Player
