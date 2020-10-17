{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Wordcrab.PlayResult where

import Control.Lens (makeLenses)
import Data.Aeson (FromJSON, ToJSON)
import Data.List.NonEmpty (NonEmpty)
import GHC.Generics (Generic)

import Wordcrab.Board (Board, TileInPlay)
import Wordcrab.Tiles (PlayedTile)

data PlayResult = PlayResult
  { _newBoard :: Board PlayedTile
  , _mainWord :: NonEmpty (TileInPlay PlayedTile)
  , _perpendicularWords :: [[TileInPlay PlayedTile]]
  , _score :: Integer
  }
  deriving (Generic)
instance ToJSON PlayResult
instance FromJSON PlayResult

makeLenses ''PlayResult
