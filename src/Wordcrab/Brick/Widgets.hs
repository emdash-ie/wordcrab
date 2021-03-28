{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Brick Widgets for use in Wordcrab UIs
module Wordcrab.Brick.Widgets (
  rack,
  board,
  scoreBox,
  message,
  inProgress,
  waitingRoom,
) where

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
import qualified Wordcrab.Client as Client
import Wordcrab.GameState (
  GameState (..),
  JoinError (..),
  players,
  tiles,
  toPreviewState,
 )
import qualified Wordcrab.GameState as GameState
import qualified Wordcrab.GameState.Event as GameState.Event
import Wordcrab.PlayResult (
  PlayResult (..),
  mainWord,
  newBoard,
  perpendicularWords,
 )
import qualified Wordcrab.PlayResult as PlayResult
import qualified Wordcrab.Player as Player
import Wordcrab.Room (Room (..))
import qualified Wordcrab.Tiles as Tiles

waitingRoom :: Room -> [Brick.Widget ()]
waitingRoom Room{..} =
  [ Brick.str
      ( "In the waiting room. Last player to join was: "
          <> show _nextPlayerId
      )
  ]

inProgress :: Client.InProgress -> [Brick.Widget ()]
inProgress s =
  pure $
    center $
      Brick.joinBorders $
        Brick.vBox
          [ board s
          , rack
              ( s
                  ^. Client.preview . Client.gameState . GameState.players
                    . GameState.Event.currentPlayer
                    . Player.rack
              )
              (s ^. Client.rackCursor)
          , scoreBox
              (s ^. Client.current . players)
              (s ^. Client.preview . Client.playResult)
          , message (s ^. Client.messages)
          ]

rack :: [Tiles.Tile] -> Maybe Int -> Brick.Widget ()
rack ts cursor =
  Brick.padTop (Brick.Pad 1) $
    Brick.str "Your rack:"
      <=> maybeCursor (Brick.hBox (fmap (border . Brick.str . tile) ts))
 where
  tile Tiles.Blank = " "
  tile (Tiles.Letter lt) = pure (Tiles.letter lt)
  maybeCursor = case cursor of
    Just c -> Brick.showCursor () (Brick.Location (1 + (3 * c), 1))
    Nothing -> id

scoreBox :: GameState.Event.Players -> Maybe PlayResult -> Brick.Widget n
scoreBox ps result =
  Brick.vBox $
    ( \(i, p) ->
        Brick.hLimit 61 $
          Brick.strWrap $
            playerNameString i
              <> if p == ps ^. GameState.Event.currentPlayer
                then scoreString p <> " (" <> moveString result <> ")"
                else scoreString p
    )
      <$> zip [1 ..] (NE.toList $ GameState.Event.turnOrder ps)

playerNameString i = "Player " <> show i <> ": "

tile :: Board.Square (Maybe Tiles.PlayedTile) -> Brick.Widget n
tile (Board.Square st mt) =
  let withAttr = case st of
        Board.Normal -> id
        Board.WordMultiplier 2 -> Brick.withAttr Attributes.doubleWord
        Board.WordMultiplier 3 -> Brick.withAttr Attributes.tripleWord
        Board.LetterMultiplier 2 -> Brick.withAttr Attributes.doubleLetter
        Board.LetterMultiplier 3 -> Brick.withAttr Attributes.tripleLetter
        _ -> id
   in withAttr $
        Brick.hLimit 3 $
          center $
            Brick.str $ case mt of
              Nothing -> " "
              Just t -> case t of
                Tiles.PlayedBlank c -> [c]
                Tiles.PlayedLetter lt -> [Tiles.letter lt]

board s =
  let c =
        Brick.showCursor
          ()
          ( Brick.Location $
              s ^. Client.boardCursor
                & _1 *~ 4
                & _1 +~ 2
                & _2 *~ 2
                & _2 +~ 1
          )
      b =
        fromRight
          (s ^. Client.preview . Client.displayBoard)
          (s ^. Client.preview . Client.gameState . GameState.board)
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
                      . fmap tile
                      . Board.unRow
                  )
                  rs
   in if isJust (s ^. Client.rackCursor)
        then w
        else c w

message messages = Brick.txt $ fromMaybe "" $ listToMaybe messages

scoreString p = show (p ^. Player.score)

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
