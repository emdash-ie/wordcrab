{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
module Wordcrab.Player where

import Control.Lens (makeLenses)

import qualified Wordcrab.Tiles as Tiles
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)

data Player = Player
  { _score :: Integer
  , _rack :: [Tiles.Tile]
  } deriving (Eq, Generic)
instance ToJSON Player
instance FromJSON Player

makeLenses ''Player
