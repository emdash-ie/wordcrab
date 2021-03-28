{-# LANGUAGE DeriveGeneric #-}

module Wordcrab.GameState.Play where

import Data.Aeson (FromJSON, ToJSON)
import Data.List.NonEmpty (NonEmpty)
import GHC.Generics (Generic)

import qualified Wordcrab.Board as Board
import qualified Wordcrab.Tiles as Tiles

data Play = Play
  { position :: Board.Position
  , direction :: Board.Direction
  , tiles :: NonEmpty Tiles.PlayedTile
  }
  deriving (Show, Generic)
instance ToJSON Play
instance FromJSON Play
