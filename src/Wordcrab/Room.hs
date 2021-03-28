{-# LANGUAGE DeriveGeneric #-}

-- | Rooms which players can join, in which games can be played.
module Wordcrab.Room where

import Data.Aeson (FromJSON, ToJSON)
import Data.Bifunctor (second)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import GHC.Generics (Generic)
import qualified Wordcrab.Player as Player
import qualified Wordcrab.Player.Waiting as Player.Waiting

fold :: Maybe Room -> [Event] -> Either FoldError (Maybe Room)
fold room [] = Right room
fold room (event : events) = do
  newRoom <- processEvent room event
  fold newRoom events

processEvent :: Maybe Room -> Event -> Either FoldError (Maybe Room)
processEvent state event = case (state, event) of
  (Nothing, Leave p) -> Left (PlayerNotFound p)
  (room, Join p) -> Right (Just (join room p))
  (Just room, Leave p) ->
    if p == Player.Waiting._id (fst (_players room))
      then case Map.lookupMin (snd (_players room)) of
        Nothing -> Right Nothing
        Just (_, player) ->
          Right
            ( Just
                ( room
                    { _players =
                        ( player
                        , Map.delete (Player.Waiting._id player) (snd (_players room))
                        )
                    }
                )
            )
      else case Map.lookup p (snd (_players room)) of
        Just _ -> Right (Just (room{_players = second (Map.delete p) (_players room)}))
        Nothing -> Left (PlayerNotFound p)

data FoldError = PlayerNotFound Player.Id deriving (Show)

data Room = Room
  { _players :: (Player.Waiting, Map Player.Id Player.Waiting)
  , _nextPlayerId :: Player.Id
  }
  deriving (Show, Generic)

instance ToJSON Room

instance FromJSON Room

join :: Maybe Room -> Player.Id -> Room
join Nothing pId =
  Room
    { _players = (Player.Waiting pId, Map.empty)
    , _nextPlayerId = Player.nextId pId
    }
join (Just r) pId =
  let (lastPlayer, players) = _players r
   in Room
        { _players =
            ( Player.Waiting pId
            , Map.insert (Player.Waiting._id lastPlayer) lastPlayer players
            )
        , _nextPlayerId = Player.nextId pId
        }

data Event
  = Join Player.Id
  | Leave Player.Id
  deriving (Eq, Show, Generic)

instance ToJSON Event

instance FromJSON Event
