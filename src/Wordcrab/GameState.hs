{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Wordcrab.GameState (Play, module Wordcrab.GameState) where

import Control.Lens (makeLenses, (%~))
import Data.Aeson (FromJSON, ToJSON)
import Data.Bifunctor (second)
import Data.Functor.Identity (Identity (..))
import Data.List.NonEmpty (NonEmpty (..), (<|))
import qualified Data.List.NonEmpty as NE
import Data.Text (Text)
import GHC.Generics (Generic)

import Wordcrab.Board (Board)
import qualified Wordcrab.Board as Board
import Wordcrab.GameState.Event (Event)
import qualified Wordcrab.GameState.Event as Event
import Wordcrab.GameState.Play (Play (..))
import Wordcrab.PlayResult (PlayResult (..))
import Wordcrab.Player (Player (..))
import qualified Wordcrab.Player as Player
import qualified Wordcrab.Player.Waiting as Player.Waiting
import Wordcrab.Tiles (Tile)
import qualified Wordcrab.Tiles as Tiles

fold :: Maybe (GameState Identity) -> NonEmpty Event -> Either FoldError (GameState Identity)
fold state (event :| tail) = do
  newState <- processEvent state event
  case NE.nonEmpty tail of
    Nothing -> Right newState
    Just events -> fold (Just newState) events

processEvent :: Maybe (GameState Identity) -> Event -> Either FoldError (GameState Identity)
processEvent state event = case (state, event) of
  (Nothing, Event.StartGame players tiles) ->
    Right (GameState (Identity Board.blankBoard) players tiles)
  (Just gameState, Event.Play _ play) ->
    case playResult play gameState of
      Left _ ->
        Left (ErrorApplyingEvent (Just gameState) event "PlayError")
      Right (newState, _) -> Right newState
  _ -> Left (UnexpectedState state event)

data FoldError
  = UnexpectedState (Maybe (GameState Identity)) Event
  | ErrorApplyingEvent (Maybe (GameState Identity)) Event Text

data GameState m = GameState
  { _board :: m (Board Tiles.PlayedTile)
  , _players :: Event.Players
  , _tiles :: [Tiles.Tile]
  }
  deriving (Generic)
instance ToJSON (GameState Identity)
instance FromJSON (GameState Identity)
instance Show (GameState a) where
  show GameState{..} = show (_players, _tiles)

playResult ::
  Play ->
  GameState Identity ->
  Either (Board.PlayError Tiles.PlayedTile) (GameState Identity, PlayResult)
playResult
  Play{position, direction, tiles}
  gs@GameState{_board = (Identity board)} = do
    ((b, mw, pws), s) <- Board.play position direction tiles Tiles.tileScore board
    pure (gs{_board = Identity b}, PlayResult b mw pws s)

initialPlayers :: NonEmpty Player -> Event.Players
initialPlayers ps = Event.Players (NE.zip (1 :| [2 ..]) ps)

startingPlayers :: NonEmpty Player.Waiting -> [Tile] -> ([Tile], Event.Players)
startingPlayers (player :| players) tiles =
  Event.Players . NE.zip indices <$> foldr f (restOfBag, readyPlayer :| []) players
 where
  indices = 1 :| [2 ..]

  (restOfBag, readyPlayer) = unwait player tiles

  f p (ts, ps) = second (<| ps) (unwait p ts)

unwait :: Player.Waiting -> [Tile] -> ([Tile], Player)
unwait Player.Waiting{Player.Waiting._id = id} tiles =
  let player =
        Player
          { Player._id = id
          , Player._score = 0
          , Player._rack = rack
          }
      (rack, restOfBag) = splitAt 7 tiles
   in (restOfBag, player)

data JoinError = GameHasStarted deriving (Show)

data StartError
  = NotEnoughPlayers
  | GameAlreadyStarted (GameState Identity)
  deriving (Show, Generic)

instance ToJSON StartError
instance FromJSON StartError

makeLenses ''GameState

toPreviewState :: GameState Identity -> GameState (Either a)
toPreviewState = board %~ Right . runIdentity
