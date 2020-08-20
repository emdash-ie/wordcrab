{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
module Wordcrab.Server where

import Control.Monad (join)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.Trans.Reader (runReaderT, asks, ReaderT)
import Data.Aeson (FromJSON, ToJSON, toJSON)
import Data.Bifunctor (bimap)
import Data.ByteString.Lazy.Char8 (pack)
import Data.Functor.Identity (Identity(..))
import Data.List.NonEmpty (NonEmpty(..))
import Data.Proxy (Proxy(..))
import Data.Text (Text)
import qualified Data.Vector as V
import GHC.Conc (newTVar, writeTVar, readTVar, atomically, TVar)
import GHC.Generics (Generic)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Servant (throwError, (:<|>)(..), Application, HasServer(..), Server, hoistServer, serve, Post, err400, ServerError(..))
import Servant.API (ReqBody, Put, JSON, Get, (:>))
import Servant.Server (Handler)
import Wordcrab.Board (Direction(..), Position(..), PlayError, TileInPlay, Direction, Position, blankBoard, Board(..))
import qualified Wordcrab.Board as Board
import Wordcrab.GameState (initialPlayers, Players(..), GameState(..))
import Wordcrab.Player (Player (..))
import Wordcrab.PlayResult (PlayResult (..))
import Wordcrab.Tiles (PlayedTile(..), n, m, a, d, tileScore, PlayedTile)

main :: IO ()
main = do
  s <- atomically (newTVar initialState)
  run 9432 (logStdoutDev (app (ServerState s)))

type WordcrabAPI =
  "get-state" :> Get '[JSON] (GameState Identity) :<|>
  "set-state" :> ReqBody '[JSON] (GameState Identity) :> Put '[JSON] Bool :<|>
  "play" :> ReqBody '[JSON] Play :> Post '[JSON] PlayResult :<|>
  "preview" :> ReqBody '[JSON] Play :> Get '[JSON] PlayResult :<|>
  "dummy-play" :> Get '[JSON] Play

data Play = Play
  { position :: Position
  , direction :: Direction
  , tiles :: NonEmpty PlayedTile
  } deriving Generic
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
server = currentState :<|> setState :<|> play :<|> preview :<|> pure dummyPlay

wordcrabAPI :: Proxy WordcrabAPI
wordcrabAPI = Proxy

app :: ServerState -> Application
app s = serve wordcrabAPI (hoistServer wordcrabAPI (nt s) server)

data ServerState = ServerState { gameState :: TVar (GameState Identity) }

type AppM = ReaderT ServerState Handler

currentState :: AppM (GameState Identity)
currentState = do
  s <- asks gameState
  liftIO (atomically (readTVar s))

setState :: GameState Identity -> AppM Bool
setState new = do
  s <- asks gameState
  liftIO (atomically (writeTVar s new))
  pure True

play :: Play -> AppM (PlayResult)
play p = do
  s <- asks gameState
  e <- liftIO $ atomically $ do
    gs <- readTVar s
    sequence $ do
      (gs', pr) <- playResult p gs
      pure $ writeTVar s gs' >> pure pr
  -- liftIO (print (bimap show (show . toJSON) e))
  case e of
    Left e' -> throwError err400 {errBody = pack (show e')}
    Right pr -> pure pr

playResult ::
  Play ->
  GameState Identity ->
  Either (PlayError PlayedTile) (GameState Identity, PlayResult)
playResult
  Play{position, direction, tiles}
  gs@GameState{_board = (Identity board)} = do
    ((b, mw, pws), s) <- Board.play position direction tiles tileScore board
    pure (gs {_board = Identity b}, PlayResult b mw pws s)

preview :: Play -> AppM (PlayResult)
preview p = do
  s <- asks gameState
  gs <- liftIO (atomically (readTVar s))
  case playResult p gs of
    Left e -> throwError err400 {errBody = pack (show e)}
    Right (_, pr) -> pure pr

dummyPlay :: Play
dummyPlay = Play (Position 7 7) Horizontal (fmap PlayedLetter (d :| [a, m, n]))

initialState :: GameState Identity
initialState = GameState
  { _board = Identity blankBoard
  , _players = initialPlayers (Player 0 [] :| [Player 0 []])
  , _tiles = []
  }

nt :: ServerState -> AppM a -> Handler a
nt s x = runReaderT x s
