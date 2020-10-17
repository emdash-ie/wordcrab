{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}

module Wordcrab.GameState where

import Control.Lens (Lens', makeLenses, (%~))
import Data.Functor ((<&>))
import Data.Functor.Identity (Identity (..))
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Wordcrab.Board (Board)
import Wordcrab.Player (Player (..))
import qualified Wordcrab.Tiles as Tiles

data GameState m = GameState
  { _board :: m (Board Tiles.PlayedTile)
  , _players :: Players
  , _tiles :: [Tiles.Tile]
  }
  deriving (Generic)

instance ToJSON (GameState Identity)
instance FromJSON (GameState Identity)

newtype Players = Players
  { rotating :: NonEmpty (Integer, Player)
  }
  deriving (Generic)
instance ToJSON Players
instance FromJSON Players

initialPlayers :: NonEmpty Player -> Players
initialPlayers ps = Players (NE.zip (1 :| [2 ..]) ps)

nextPlayer :: Players -> Players
nextPlayer ps =
  let rotating' = case rotating ps of
        (p :| []) -> p :| []
        (p :| (p' : ps')) -> p' :| (ps' <> [p])
   in ps{rotating = rotating'}

turnOrder :: Players -> NonEmpty Player
turnOrder = fmap snd . NE.sortWith fst . rotating

currentOrder :: Players -> NonEmpty Player
currentOrder = fmap snd . rotating

currentPlayer :: Lens' Players Player
currentPlayer f (Players ((i, p) :| ps)) = f p <&> Players . (:| ps) . (,) i

makeLenses ''GameState

toPreviewState :: GameState Identity -> GameState (Either a)
toPreviewState = board %~ Right . runIdentity
