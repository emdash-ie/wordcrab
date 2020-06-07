{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
module Wordcrab.Server where

import Data.Functor.Identity (Identity(..))
import Data.List.NonEmpty (NonEmpty(..))
import Data.Proxy (Proxy(..))
import Network.Wai.Handler.Warp (run)
import Servant.API (ReqBody, Put, JSON, Get, (:>))
import Servant.Server (Handler)
import Servant ((:<|>)(..))
import Servant (Server)
import Servant (Application)
import Servant (serve)
import qualified Data.Vector as V

import Wordcrab.Board (blankBoard, Board(..))
import Wordcrab.GameState (initialPlayers, Players(..), GameState(..))
import Wordcrab.Player (Player (..))
import GHC.Conc (newTVar, writeTVar, readTVar, atomically, TVar)
import Control.Monad.Trans.Reader (runReaderT, asks, ReaderT)
import Servant (HasServer(..))
import Control.Monad.IO.Class (MonadIO(liftIO))
import Servant (hoistServer)

main :: IO ()
main = do
  s <- atomically (newTVar initialState)
  run 9432 (app (ServerState s))

type WordcrabAPI =
  "get-state" :> Get '[JSON] (GameState Identity) :<|>
  "set-state" :> ReqBody '[JSON] (GameState Identity) :> Put '[JSON] Bool

server :: ServerT WordcrabAPI AppM
server = currentState :<|> setState

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

initialState :: GameState Identity
initialState = GameState
  { _board = Identity blankBoard
  , _players = initialPlayers (Player 0 [] :| [Player 0 []])
  , _tiles = []
  }

nt :: ServerState -> AppM a -> Handler a
nt s x = runReaderT x s
