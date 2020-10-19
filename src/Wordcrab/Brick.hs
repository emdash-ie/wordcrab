{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Wordcrab.Brick where

import Brick (App (..), attrMap, defaultMain, (<=>))
import qualified Brick
import Brick.Widgets.Border (border, hBorder, vBorder)
import Brick.Widgets.Center (center)
import Control.Category ((>>>))
import Control.Lens (Lens', to, (%~), (*~), (+~), (.~), (?~), (^.), _1, _2)
import Control.Monad (join)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Aeson as JSON
import Data.Bifunctor (first, second)
import Data.ByteString.Lazy (toStrict)
import Data.Either (fromRight)
import Data.Function ((&))
import Data.Functor.Identity (Identity (..))
import Data.List (intercalate, intersperse)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, isJust, isNothing, listToMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Encoding (decodeUtf8')
import qualified Data.Vector as V
import Graphics.Vty (Event (..), Key (..), defAttr, rgbColor)
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Network.HTTP.Types (Status (..))
import Servant ((:<|>) (..))
import Servant.Client (
  BaseUrl (..),
  ClientError (..),
  ClientM,
  ResponseF (..),
  Scheme (..),
  client,
  mkClientEnv,
  runClientM,
 )
import System.Random (getStdGen)

import qualified Wordcrab.Board as Board
import qualified Wordcrab.Brick.Attributes as Attributes
import qualified Wordcrab.Brick.Widgets as Widgets
import Wordcrab.Client hiding (State)
import qualified Wordcrab.Client as Client
import Wordcrab.GameState (GameState (..), JoinError (..), board, currentPlayer, players, tiles, toPreviewState)
import qualified Wordcrab.GameState as GameState
import Wordcrab.PlayResult (PlayResult (..), mainWord, newBoard, perpendicularWords)
import qualified Wordcrab.PlayResult as PlayResult
import Wordcrab.Player (Player (..), rack, score)
import qualified Wordcrab.Player as Player
import qualified Wordcrab.Server as Server
import qualified Wordcrab.Tiles as Tiles

main :: IO ()
main = do
  gen <- getStdGen
  backend <- webBackend
  (room, pId) <- do
    e <- backendJoin backend
    case e of
      Left t -> error (show t)
      Right t -> pure t

  let app :: App Client.State e ()
      app =
        App
          { appDraw = \s -> case s of
              Waiting r -> Widgets.waitingRoom r
              Started ip -> Widgets.inProgress ip
          , appChooseCursor = Brick.showFirstCursor
          , appHandleEvent = handleEvent backend
          , appStartEvent = pure
          , appAttrMap = \s -> attributes
          }
      (startingRack1, (startingRack2, bag)) =
        splitAt 7 <$> splitAt 7 (Tiles.shuffleBag gen Tiles.tileset)

  finalState <- defaultMain app (Client.Waiting room)
  putStrLn "End of game"

webBackend :: IO (Backend IO)
webBackend = do
  env <- clientEnv
  pure $
    Backend
      { backendPreview = \p d ts _ -> do
          result <- runClientM (preview (Server.Play p d ts)) env
          pure (first fromClientError result)
      , backendPlay = \p d ts _ -> do
          result <- runClientM (play (Server.Play p d ts)) env
          pure (first fromClientError result)
      , backendJoin = do
          result <- runClientM joinGame env
          pure (first (error . show) result)
      , backendRefresh = do
          result <- runClientM getState env
          pure (first fromClientError result)
      }
 where
  getState :<|> _ :<|> play :<|> preview :<|> _ :<|> joinGame = client Server.wordcrabAPI
  httpManager = newManager defaultManagerSettings
  clientEnv = flip mkClientEnv (BaseUrl Http "localhost" 9432 "") <$> httpManager
  fromClientError :: JSON.FromJSON e => ClientError -> BackendError e
  fromClientError = \case
    FailureResponse _ r ->
      either
        (const (Client.OtherError (Text.pack (show r))))
        Client.SpecificError
        (JSON.eitherDecode (responseBody r))
    e -> Client.OtherError ("Something Servanty went wrong: " <> Text.pack (show e))

startGame :: GameState Identity -> Client.State
startGame gs =
  Started
    ( InProgress
        { _current = gs
        , _preview = initialPreview gs
        , _boardCursor = (0, 0)
        , _rackCursor = Nothing
        , _messages = []
        }
    )

initialPreview :: GameState Identity -> Preview
initialPreview gs =
  Preview
    { _gameState = gs & board %~ Right . runIdentity
    , _placed = Map.empty
    , _displayBoard = runIdentity (gs ^. board)
    , _playResult = Nothing
    }

handleEvent ::
  Backend IO ->
  Client.State ->
  Brick.BrickEvent n e ->
  Brick.EventM w (Brick.Next Client.State)
handleEvent backend s = \case
  Brick.VtyEvent (EvKey (KChar 'q') (_ : _)) -> Brick.halt s
  Brick.VtyEvent (EvKey (KChar 'r') (_ : _)) -> do
    g <- liftIO (backendRefresh backend)
    case g of
      Left _ -> Brick.continue s
      Right (GameState.Waiting r) -> Brick.continue (Waiting r)
      Right (GameState.Started gs) -> case s of
        Waiting r -> Brick.continue (startGame gs)
        Started ip -> Brick.continue (Started (ip & current .~ gs))
  Brick.VtyEvent (EvKey (KChar c) _) ->
    case s of
      Waiting _ -> Brick.halt s
      Started ip -> case c of
        ' ' -> liftIO (placeOrSelectTile backend ip) >>= Brick.continue . Started
        c -> liftIO (updateBlank backend ip c) >>= Brick.continue . Started
  Brick.VtyEvent (EvKey KRight _) -> Brick.continue $ _Started %~ moveRight $ s
  Brick.VtyEvent (EvKey KLeft _) -> Brick.continue $ _Started %~ moveLeft $ s
  Brick.VtyEvent (EvKey KDown _) -> Brick.continue $ _Started %~ moveDown $ s
  Brick.VtyEvent (EvKey KUp _) -> Brick.continue (_Started %~ moveUp $ s)
  Brick.VtyEvent (EvKey KEnter _) -> case s of
    Waiting _ -> Brick.continue s
    Started p -> do
      p <- liftIO (confirmPlay backend p)
      Brick.continue (Started p)
  Brick.VtyEvent (EvKey KBS _) -> case s of
    Waiting _ -> Brick.continue s
    Started p -> do
      e <- liftIO $ sequence (pickUpTile backend p)
      Brick.continue (Started (either (message p . Text.pack) id e))
  _ -> Brick.continue s

attributes =
  Brick.attrMap
    defAttr
    [ (Attributes.doubleWord, Brick.bg (rgbColor 133 182 255))
    , (Attributes.tripleWord, Brick.bg (rgbColor 255 156 156))
    , (Attributes.doubleLetter, Brick.bg (rgbColor 148 219 255))
    , (Attributes.tripleLetter, Brick.bg (rgbColor 255 201 239))
    ]
