{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}

module Wordcrab.Server where

import Control.Category ((>>>))
import Control.Monad ((>=>))
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans.Reader (ReaderT, asks, runReaderT)
import Data.Aeson (FromJSON, ToJSON)
import Data.Bifunctor (first)
import Data.ByteString.Lazy (fromStrict, toStrict)
import Data.ByteString.Lazy.Char8 (pack)
import Data.Functor.Identity (Identity (..))
import Data.List.NonEmpty (NonEmpty (..), nonEmpty, (<|))
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Encoding (encodeUtf8)
import GHC.Conc (
  TVar,
  atomically,
  newTVar,
  newTVarIO,
  readTVar,
  readTVarIO,
  writeTVar,
 )
import GHC.Generics (Generic)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Servant (
  Application,
  HasServer (..),
  Post,
  ServerError (..),
  err400,
  err500,
  hoistServer,
  serve,
  throwError,
  (:<|>) (..),
 )
import Servant.API (Get, JSON, Put, ReqBody, (:>))
import Servant.Server (Handler)
import System.Random (getStdGen)

import Wordcrab.API (
  AttributedPlay (..),
  PlayError (..),
  PlayResponse (..),
  StartGameResponse (..),
  WordcrabAPI,
 )
import Wordcrab.Board (Direction (..), Position (..), blankBoard)
import qualified Wordcrab.Board as Board
import Wordcrab.GameState (GameState (..), StartError (..))
import qualified Wordcrab.GameState as GameState
import qualified Wordcrab.GameState.Event as Event
import qualified Wordcrab.GameState.Event as GameState.Event
import Wordcrab.PlayResult (PlayResult (..))
import qualified Wordcrab.Player as Player
import Wordcrab.Room (Room)
import qualified Wordcrab.Room as Room
import Wordcrab.Tiles (PlayedTile (..), a, d, m, n, tileScore)
import qualified Wordcrab.Tiles as Tiles

main :: IO ()
main = do
  roomVar <- newTVarIO []
  gameVar <- newTVarIO []
  run 9432 (logStdoutDev (app (ServerState roomVar gameVar)))

server :: ServerT WordcrabAPI AppM
server =
  currentState
    :<|> play
    :<|> preview
    :<|> joinRoom
    :<|> startGame

wordcrabAPI :: Proxy WordcrabAPI
wordcrabAPI = Proxy

app :: ServerState -> Application
app s = serve wordcrabAPI (hoistServer wordcrabAPI (nt s) server)

data ServerState = ServerState
  { -- | The events so far in the game, which taken together make up the game
    -- state. Most recent first.
    gameEvents :: TVar [GameState.Event.Event]
  , -- | The events so far in the room, which make up the room state. Most
    -- recent first.
    roomEvents :: TVar [Room.Event]
  }

type AppM = ReaderT ServerState Handler

currentState :: AppM (Maybe (GameState Identity))
currentState = do
  tvar <- asks gameEvents
  es <- liftIO (readTVarIO tvar)
  case NE.nonEmpty es of
    Nothing -> pure Nothing
    Just events -> case GameState.fold Nothing events of
      Left e -> throwError err500
      Right gs -> pure (Just gs)

play :: AttributedPlay -> AppM (PlayResponse PlayError)
play AttributedPlay{..} = do
  eventsVar <- asks gameEvents
  (atomically >>> liftIO >=> traverse throwUnhandleable) do
    events <- readTVar eventsVar
    case playWithEvents unattributedPlay events of
      response@(PlaySuccessful _) -> do
        writeTVar eventsVar (Event.Play player unattributedPlay : events)
        pure response
      response@(PlayUnsuccessful _) -> pure response

preview :: GameState.Play -> AppM (PlayResponse PlayError)
preview play = do
  gameEvents <- asks gameEvents >>= (readTVarIO >>> liftIO)
  case playWithEvents play gameEvents of
    PlayUnsuccessful e -> PlayUnsuccessful <$> throwUnhandleable e
    PlaySuccessful x -> pure (PlaySuccessful x)

playWithEvents :: GameState.Play -> [GameState.Event.Event] -> PlayResponse (HandleableError PlayError)
playWithEvents play events =
  either PlayUnsuccessful PlaySuccessful $ do
    events <- note (HandleableError NoGameInProgress) (NE.nonEmpty events)
    currentState <- first (describeFoldError >>> UnhandleableError) (GameState.fold Nothing events)
    snd <$> first (PlayError >>> HandleableError) (GameState.playResult play currentState)

note :: a -> Maybe b -> Either a b
note a = maybe (Left a) Right

joinRoom :: AppM (Room, Player.Id)
joinRoom = do
  events <- asks roomEvents
  result <- liftIO $
    atomically $ do
      es <- readTVar events
      case Room.fold Nothing es of
        Right room -> do
          let playerId = maybe Player.firstId (Room._nextPlayerId >>> Player.nextId) room
          let newEvent = Room.Join playerId
          let newEvents = newEvent : es
          case Room.processEvent room newEvent of
            Right (Just newRoom) -> do
              writeTVar events newEvents
              pure (Right (newRoom, playerId))
            e -> pure (Left (show e))
        Left e -> pure (Left (show e))
  case result of
    Right t -> pure t
    Left message -> throwError err400{errBody = pack message}

startGame :: AppM (StartGameResponse StartError)
startGame = do
  gameVar <- asks gameEvents
  roomVar <- asks roomEvents
  gen <- liftIO getStdGen
  (atomically >>> liftIO >=> traverse throwUnhandleable) $ do
    gameEvents <- readTVar gameVar
    case gameEvents of
      [] -> do
        roomEvents <- readTVar roomVar
        case Room.fold Nothing roomEvents of
          Right (Just Room.Room{..}) ->
            if Map.null (snd _players)
              then pure (GameNotStarted (HandleableError NotEnoughPlayers))
              else do
                let players = fst _players :| Map.elems (snd _players)
                let tiles = Tiles.shuffleBag gen Tiles.tileset
                let (restOfBag, readyPlayers) = GameState.startingPlayers players tiles
                let newState =
                      GameState
                        { _board = Identity blankBoard
                        , _players = readyPlayers
                        , _tiles = restOfBag
                        }
                writeTVar gameVar (Event.StartGame readyPlayers tiles : gameEvents)
                pure (GameStarted newState)
      event : events -> pure $ either GameNotStarted GameStarted do
        gameState <-
          first
            (describeFoldError >>> UnhandleableError)
            (GameState.fold Nothing (event :| events))
        Left (HandleableError (GameAlreadyStarted gameState))

data HandleableError e
  = HandleableError e
  | UnhandleableError Text

throwUnhandleable :: HandleableError e -> AppM e
throwUnhandleable = \case
  HandleableError e -> pure e
  UnhandleableError message ->
    throwError err500{errBody = fromStrict (encodeUtf8 message)}

describeFoldError :: GameState.FoldError -> Text
describeFoldError = \case
  GameState.UnexpectedState state event ->
    "Unexpected state (" <> Text.pack (show state)
      <> ") when applying event: "
      <> Text.pack (show event)
  GameState.ErrorApplyingEvent _state event text ->
    "Couldn't apply event ("
      <> Text.pack (show event)
      <> ") because: "
      <> text

nt :: ServerState -> AppM a -> Handler a
nt s x = runReaderT x s
