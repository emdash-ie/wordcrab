{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}

module Wordcrab.GameState where

import Control.Lens (Lens', makeLenses, (%~))
import Data.Functor ((<&>))
import Data.Functor.Identity (Identity (..))
import Data.List.NonEmpty (NonEmpty (..), (<|))
import qualified Data.List.NonEmpty as NE
import Data.Maybe (fromMaybe)

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Wordcrab.Board (Board)
import Wordcrab.Player (Player (..))
import qualified Wordcrab.Player as Player
import qualified Wordcrab.Tiles as Tiles

data Game m = Waiting Room | Started (GameState m) deriving (Generic)
instance ToJSON (Game Identity)
instance FromJSON (Game Identity)

data Room = Room
  { _waitingPlayers :: [Player.Waiting]
  , _lastPlayerId :: Player.Id
  }
  deriving (Show, Generic)
instance ToJSON Room
instance FromJSON Room

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
  deriving (Generic, Show)
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

join :: Room -> Room
join r =
  let pId = Player.nextId (_lastPlayerId r)
   in r
        { _waitingPlayers = (_waitingPlayers r) <> [Player.Waiting pId]
        , _lastPlayerId = pId
        }

data JoinError = GameHasStarted deriving (Show)

addPlayer :: Player -> Players -> Players
addPlayer p ps =
  let f t@(i, _) (last, list) =
        ( Just i
        , t :
          if i > fromMaybe (i + 1) last
            then (i + 1, p) : list
            else list
        )
      (_, x : list) = foldr f (Nothing, []) (rotating ps)
   in ps{rotating = x :| list}

emptyRoom :: Room
emptyRoom =
  Room
    { _waitingPlayers = []
    , _lastPlayerId = Player.firstId
    }

makeLenses ''GameState

toPreviewState :: GameState Identity -> GameState (Either a)
toPreviewState = board %~ Right . runIdentity
