{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

{- | Types that define the Wordcrab API, which servers and clients each
 implement to communicate with one another.
-}
module Wordcrab.API where

import Data.Aeson (FromJSON, ToJSON)
import Data.Functor.Identity (Identity)
import GHC.Generics (Generic)
import Servant (Get, JSON, Post, Put, ReqBody, (:<|>), type (:>))

import qualified Wordcrab.Board as Board
import Wordcrab.GameState (GameState, StartError)
import qualified Wordcrab.GameState as GameState
import Wordcrab.PlayResult (PlayResult)
import qualified Wordcrab.Player as Player
import Wordcrab.Room (Room)
import qualified Wordcrab.Tiles as Tiles

type WordcrabAPI =
  "get-state" :> Get '[JSON] (Maybe (GameState Identity))
    :<|> "play" :> ReqBody '[JSON] AttributedPlay :> Post '[JSON] (PlayResponse PlayError)
    :<|> "preview" :> ReqBody '[JSON] GameState.Play :> Get '[JSON] (PlayResponse PlayError)
    :<|> "join" :> Post '[JSON] (Room, Player.Id)
    :<|> "start" :> Put '[JSON] (StartGameResponse StartError)

data PlayResponse e
  = -- | Your play was successful, here is the result.
    PlaySuccessful PlayResult
  | -- | Your play was unsuccessful, here is the reason.
    PlayUnsuccessful e
  deriving (Generic, Functor, Foldable, Traversable)

instance ToJSON (PlayResponse PlayError)
instance FromJSON (PlayResponse PlayError)

data PlayError
  = -- | You can't play, no game is in progress.
    NoGameInProgress
  | -- | There is some logical error with your play.
    PlayError (Board.PlayError Tiles.PlayedTile)
  deriving (Generic)

instance ToJSON PlayError
instance FromJSON PlayError

data AttributedPlay = AttributedPlay
  { player :: Player.Id
  , unattributedPlay :: GameState.Play
  }
  deriving (Generic)

instance FromJSON AttributedPlay
instance ToJSON AttributedPlay

data StartGameResponse e
  = GameStarted (GameState Identity)
  | GameNotStarted e
  deriving (Generic, Functor, Foldable, Traversable)

instance ToJSON (StartGameResponse StartError)
instance FromJSON (StartGameResponse StartError)
