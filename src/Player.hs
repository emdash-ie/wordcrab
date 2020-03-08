module Player (Player(..)) where

import qualified Tiles

data Player = Player
  { score :: Integer
  , rack :: [Tiles.Tile]
  }
