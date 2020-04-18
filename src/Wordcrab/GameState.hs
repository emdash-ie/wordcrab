{-# LANGUAGE TemplateHaskell #-}
module Wordcrab.GameState where

import Control.Lens (makeLenses, (%~), Lens')
import Data.Functor ((<&>))
import Data.Functor.Identity (Identity(..))
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE

import Wordcrab.Board (Board)
import qualified Wordcrab.Tiles as Tiles
import Wordcrab.Player (Player(..))

data GameState m = GameState
  { _board :: m (Board Tiles.PlayedTile)
  , _players :: Players
  , _tiles :: [Tiles.Tile]
  }

newtype Players = Players
  { rotating :: NonEmpty (Integer, Player)
  }

initialPlayers :: NonEmpty Player -> Players
initialPlayers ps = Players (NE.zip (1 :| [2..]) ps)

nextPlayer :: Players -> Players
nextPlayer ps = let
  rotating' = case rotating ps of
    (p :| []) -> p :| []
    (p :| (p' : ps')) -> p' :| (ps' <> [p])
  in ps { rotating = rotating' }

turnOrder :: Players -> NonEmpty Player
turnOrder = fmap snd . NE.sortWith fst . rotating

currentOrder :: Players -> NonEmpty Player
currentOrder = fmap snd . rotating

currentPlayer :: Lens' Players Player
currentPlayer f (Players ((i, p) :| ps)) = f p <&> Players . (:| ps) . (,) i

makeLenses ''GameState

toPreviewState :: GameState Identity -> GameState (Either a)
toPreviewState = board %~ Right . runIdentity
