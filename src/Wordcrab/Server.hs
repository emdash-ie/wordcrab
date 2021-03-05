{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Wordcrab.Server where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans.Reader (ReaderT, asks, runReaderT)
import Data.Aeson (FromJSON, ToJSON)
import Data.Bifunctor (first)
import Data.ByteString.Lazy.Char8 (pack)
import Data.Functor.Identity (Identity (..))
import Data.List.NonEmpty (NonEmpty (..), nonEmpty)
import Data.Proxy (Proxy (..))
import GHC.Conc (TVar, atomically, newTVar, readTVar, readTVarIO, writeTVar)
import GHC.Generics (Generic)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Servant (Application, HasServer (..), Post, ServerError (..), err400, hoistServer, serve, throwError, (:<|>) (..))
import Servant.API (Get, JSON, Put, ReqBody, (:>))
import Servant.Server (Handler)
import System.Random (getStdGen)

import Wordcrab.Board (Direction (..), PlayError, Position (..), blankBoard)
import qualified Wordcrab.Board as Board
import Wordcrab.GameState (Game (..), GameState (..), StartError (..), emptyRoom, _lastPlayerId)
import qualified Wordcrab.GameState as GameState
import Wordcrab.PlayResult (PlayResult (..))
import qualified Wordcrab.Player as Player
import Wordcrab.Tiles (PlayedTile (..), a, d, m, n, tileScore)
import qualified Wordcrab.Tiles as Tiles

main :: IO ()
main = do
  s <- atomically (newTVar (Waiting emptyRoom))
  run 9432 (logStdoutDev (app (ServerState s)))

type WordcrabAPI =
  "get-state" :> Get '[JSON] (Game Identity)
    :<|> "set-state" :> ReqBody '[JSON] (Game Identity) :> Put '[JSON] Bool
    :<|> "play" :> ReqBody '[JSON] Play :> Post '[JSON] PlayResult
    :<|> "preview" :> ReqBody '[JSON] Play :> Get '[JSON] PlayResult
    :<|> "dummy-play" :> Get '[JSON] Play
    :<|> "join" :> Post '[JSON] (GameState.Room, Player.Id)
    :<|> "start" :> Put '[JSON] (Either StartError (GameState Identity))

data Play = Play
  { position :: Position
  , direction :: Direction
  , tiles :: NonEmpty PlayedTile
  }
  deriving (Generic)
instance ToJSON Play
instance FromJSON Play

-- data PlayResult = PlayResult
--   { newBoard :: Board PlayedTile
--   , mainWord :: NonEmpty (TileInPlay PlayedTile)
--   , perpendicularWords :: [[TileInPlay PlayedTile]]
--   , score :: Integer
--   } deriving Generic
-- instance ToJSON PlayResult
-- instance FromJSON PlayResult

server :: ServerT WordcrabAPI AppM
server =
  currentState :<|> setState :<|> play :<|> preview :<|> pure dummyPlay
    :<|> joinGame
    :<|> startGame

wordcrabAPI :: Proxy WordcrabAPI
wordcrabAPI = Proxy

app :: ServerState -> Application
app s = serve wordcrabAPI (hoistServer wordcrabAPI (nt s) server)

newtype ServerState = ServerState {gameState :: TVar (Game Identity)}

type AppM = ReaderT ServerState Handler

currentState :: AppM (Game Identity)
currentState = do
  s <- asks gameState
  liftIO (readTVarIO s)

setState :: Game Identity -> AppM Bool
setState new = do
  s <- asks gameState
  liftIO (atomically (writeTVar s new))
  pure True

play :: Play -> AppM PlayResult
play p = do
  s <- asks gameState
  result <- liftIO $
    atomically $ do
      g <- readTVar s
      case g of
        Waiting _ -> pure (Left "Can't play before the game has started")
        Started gs -> sequence $ do
          (gs', pr) <- first show (playResult p gs)
          pure $ writeTVar s (Started gs') >> pure pr
  -- liftIO (print (bimap show (show . toJSON) e))
  case result of
    Left e -> throwError err400{errBody = pack e}
    Right pr -> pure pr

playResult ::
  Play ->
  GameState Identity ->
  Either (PlayError PlayedTile) (GameState Identity, PlayResult)
playResult
  Play{position, direction, tiles}
  gs@GameState{_board = (Identity board)} = do
    ((b, mw, pws), s) <- Board.play position direction tiles tileScore board
    pure (gs{_board = Identity b}, PlayResult b mw pws s)

preview :: Play -> AppM PlayResult
preview p = do
  s <- asks gameState
  g <- liftIO (readTVarIO s)
  case g of
    Waiting _ ->
      throwError err400{errBody = "Can't play before the game has started"}
    Started gs -> case playResult p gs of
      Left e -> throwError err400{errBody = pack (show e)}
      Right (_, pr) -> pure pr

dummyPlay :: Play
dummyPlay = Play (Position 7 7) Horizontal (fmap PlayedLetter (d :| [a, m, n]))

joinGame :: AppM (GameState.Room, Player.Id)
joinGame = do
  s <- asks gameState
  result <- liftIO $
    atomically $ do
      g <- readTVar s
      case g of
        Waiting r -> do
          let r' = GameState.join r
          writeTVar s (Waiting r')
          pure (Right (r', _lastPlayerId r'))
        Started _ -> pure (Left "Can't join a game once it has started")
  case result of
    Right t -> pure t
    Left message -> throwError err400{errBody = message}

startGame :: AppM (Either StartError (GameState Identity))
startGame = do
  s <- asks gameState
  gen <- liftIO getStdGen
  liftIO $
    atomically $ do
      g <- readTVar s
      case g of
        Waiting room ->
          case nonEmpty (GameState._waitingPlayers room) of
            Nothing -> pure (Left NotEnoughPlayers)
            Just players -> do
              let tiles = Tiles.shuffleBag gen Tiles.tileset
              let (restOfBag, readyPlayers) = GameState.startingPlayers players tiles
              let newState =
                    GameState
                      { _board = Identity blankBoard
                      , _players = readyPlayers
                      , _tiles = restOfBag
                      }
              writeTVar s (Started newState)
              pure (Right newState)
        Started gameState -> pure (Left (GameAlreadyStarted gameState))

nt :: ServerState -> AppM a -> Handler a
nt s x = runReaderT x s
