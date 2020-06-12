{-# LANGUAGE TemplateHaskell #-}
module Wordcrab.PlayResult where

import Control.Lens (makeLenses)
import Data.List.NonEmpty (NonEmpty)

import Wordcrab.Board (Board, TileInPlay)
import Wordcrab.Tiles (PlayedTile)

data PlayResult = PlayResult
  { _newBoard :: Board PlayedTile
  , _mainWord :: NonEmpty (TileInPlay PlayedTile)
  , _perpendicularWords :: [[TileInPlay PlayedTile]]
  , _score :: Integer
  }

makeLenses ''PlayResult
