{-# LANGUAGE DeriveGeneric #-}

-- | Events for the event sourcing
module Wordcrab.GameState.Event where

import Control.Lens (Lens')
import Data.Aeson (FromJSON, ToJSON)
import Data.Functor ((<&>))
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Maybe (fromMaybe)
import GHC.Generics (Generic)

import qualified Wordcrab.GameState.Play as Play
import Wordcrab.Player (Player)
import qualified Wordcrab.Player as Player
import qualified Wordcrab.Tiles as Tiles

data Event
  = StartGame Players [Tiles.Tile]
  | Play Player.Id Play.Play
  deriving (Show, Generic)

instance ToJSON Event
instance FromJSON Event

newtype Players = Players
  { rotating :: NonEmpty (Integer, Player)
  }
  deriving (Generic, Show)
instance ToJSON Players
instance FromJSON Players

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
