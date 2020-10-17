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
          { appDraw = draw
          , appChooseCursor = Brick.showFirstCursor
          , appHandleEvent = handleEvent backend
          , appStartEvent = pure
          , appAttrMap = \s -> attributes
          }
      (startingRack1, (startingRack2, bag)) =
        splitAt 7 <$> splitAt 7 (Tiles.shuffleBag gen Tiles.tileset)
      initialState = Client.Waiting room
      draw :: Client.State -> [Brick.Widget ()]
      draw s = case s of
        Waiting r -> drawWaitingRoom r
        Started ip -> drawInProgress ip

      drawWaitingRoom :: GameState.Room -> [Brick.Widget ()]
      drawWaitingRoom r =
        [ Brick.str
            ( "In the waiting room. Last player to join was: "
                <> show (GameState._lastPlayerId r)
            )
        ]
      drawInProgress :: Client.InProgress -> [Brick.Widget ()]
      drawInProgress s =
        let boardWidget =
              let c =
                    Brick.showCursor
                      ()
                      ( Brick.Location $
                          s ^. boardCursor
                            & _1 *~ 4
                            & _1 +~ 2
                            & _2 *~ 2
                            & _2 +~ 1
                      )
                  b =
                    fromRight
                      (s ^. preview . displayBoard)
                      (s ^. preview . gameState . board)
                  rs = V.toList (Board.unBoard b)
                  width = (4 * length rs) + 1
                  w =
                    Brick.hLimit width $
                      border $
                        Brick.vBox $
                          intersperse hBorder $
                            fmap
                              ( Brick.vLimit 1 . Brick.hBox
                                  . intersperse vBorder
                                  . V.toList
                                  . fmap tileWidget
                                  . Board.unRow
                              )
                              rs
                  tileWidget :: Board.Square (Maybe Tiles.PlayedTile) -> Brick.Widget n
                  tileWidget (Board.Square st mt) =
                    let withAttr = case st of
                          Board.Normal -> id
                          Board.WordMultiplier 2 -> Brick.withAttr doubleWord
                          Board.WordMultiplier 3 -> Brick.withAttr tripleWord
                          Board.LetterMultiplier 2 -> Brick.withAttr doubleLetter
                          Board.LetterMultiplier 3 -> Brick.withAttr tripleLetter
                          _ -> id
                     in withAttr $
                          Brick.hLimit 3 $
                            center $
                              Brick.str $ case mt of
                                Nothing -> " "
                                Just t -> case t of
                                  Tiles.PlayedBlank c -> [c]
                                  Tiles.PlayedLetter lt -> [Tiles.letter lt]
               in if isJust (s ^. rackCursor)
                    then w
                    else c w
            rackWidget' =
              Brick.padTop (Brick.Pad 1) $
                Brick.str "Your rack:"
                  <=> rackWidget
                    (s ^. preview . gameState . players . currentPlayer . rack)
                    (s ^. rackCursor)
            scoreBoxWidget :: GameState.Players -> Maybe PlayResult -> Brick.Widget n
            scoreBoxWidget ps result =
              Brick.vBox $
                ( \(i, p) ->
                    Brick.hLimit 61 $
                      Brick.strWrap $
                        playerNameString i
                          <> if p == ps ^. currentPlayer
                            then scoreString p <> " (" <> moveString result <> ")"
                            else scoreString p
                )
                  <$> zip [1 ..] (NE.toList $ GameState.turnOrder ps)
            playerNameString i = "Player " <> show i <> ": "
            moveString :: Maybe PlayResult -> String
            moveString result = case result of
              Nothing -> "Error"
              Just pr ->
                "-> " <> show (pr ^. PlayResult.score) <> ": " <> showWord (NE.toList (pr ^. mainWord))
                  <> ", "
                  <> intercalate ", " (fmap showWord (pr ^. perpendicularWords))
            showWord :: [Board.TileInPlay Tiles.PlayedTile] -> String
            showWord w =
              let multiplier tip = case Board.squareType <$> tip of
                    (Board.PlayedNow, Board.WordMultiplier n) -> n
                    _ -> 1
               in show (product (fmap multiplier w))
                    <> " x "
                    <> concatMap showTile w
            showTile :: Board.TileInPlay Tiles.PlayedTile -> String
            showTile (when, square) = show $ case Board.squareContents square of
              Tiles.PlayedLetter lt ->
                ( Tiles.letter lt
                , Board.tileMultiplier when (Board.squareType square)
                    * fromIntegral (Tiles.score lt)
                )
              Tiles.PlayedBlank c -> (c, 0)
            scoreString p = show (p ^. Player.score)
            messageWidget = Brick.txt $ fromMaybe "" $ listToMaybe $ s ^. messages
         in pure $
              center $
                Brick.joinBorders $
                  Brick.vBox
                    [ boardWidget
                    , rackWidget'
                    , scoreBoxWidget (s ^. current . players) (s ^. preview . playResult)
                    , messageWidget
                    ]
      doubleWord = Brick.attrName "doubleWord"
      tripleWord = Brick.attrName "tripleWord"
      doubleLetter = Brick.attrName "doubleLetter"
      tripleLetter = Brick.attrName "tripleLetter"
      attributes =
        Brick.attrMap
          defAttr
          [ (doubleWord, Brick.bg (rgbColor 133 182 255))
          , (tripleWord, Brick.bg (rgbColor 255 156 156))
          , (doubleLetter, Brick.bg (rgbColor 148 219 255))
          , (tripleLetter, Brick.bg (rgbColor 255 201 239))
          ]
  finalState <- defaultMain app initialState
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

rackWidget :: [Tiles.Tile] -> Maybe Int -> Brick.Widget ()
rackWidget ts cursor =
  maybeCursor $ Brick.hBox $ fmap (border . Brick.str . tileWidget) ts
 where
  tileWidget Tiles.Blank = " "
  tileWidget (Tiles.Letter lt) = pure (Tiles.letter lt)
  maybeCursor = case cursor of
    Just c -> Brick.showCursor () (Brick.Location (1 + (3 * c), 1))
    Nothing -> id
