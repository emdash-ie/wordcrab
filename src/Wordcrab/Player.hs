{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Wordcrab.Player (
  Player (..),
  Wordcrab.Player.id,
  rack,
  score,
  Id (..),
  firstId,
  nextId,
  Waiting (Waiting),
) where

import Control.Lens (makeLenses)
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import qualified Wordcrab.Tiles as Tiles

import Wordcrab.Player.Id
import Wordcrab.Player.Waiting (Waiting (Waiting))
import qualified Wordcrab.Tiles as Tiles

data Player = Player
  { _id :: Id
  , _score :: Integer
  , _rack :: [Tiles.Tile]
  }
  deriving (Eq, Generic, Show)
instance ToJSON Player
instance FromJSON Player

makeLenses ''Player
