{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Wordcrab.Brick where

import Brick (App(..), defaultMain, attrMap, (<=>))
import qualified Brick
import Brick.Widgets.Border (border, hBorder, vBorder)
import Brick.Widgets.Center (center)
import Control.Category ((>>>))
import Control.Monad (join)
import Control.Monad.IO.Class (liftIO)
import Control.Lens ((^.), (*~), _1, _2, to, (.~), (+~), (%~), (?~), Lens')
import Data.Bifunctor (first, second)
import Data.Either (fromRight)
import Data.Function ((&))
import Data.Functor.Identity (Identity(..))
import Data.List (intersperse, intercalate)
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust, isNothing, listToMaybe, fromMaybe)
import qualified Data.Text as Text
import Data.Text (Text)
import qualified Data.Vector as V
import Graphics.Vty (defAttr, Key(..), Event(..), rgbColor)
import Prelude hiding (lookup)
import System.Random (getStdGen)
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Servant ((:<|>)(..))
import Servant.Client (client, ClientM, runClientM, BaseUrl(..), Scheme(..), mkClientEnv)

import qualified Wordcrab.Board as Board
import qualified Wordcrab.Tiles as Tiles
import Wordcrab.Player (Player(..), score, rack)
import qualified Wordcrab.Player as Player
import Wordcrab.PlayResult (PlayResult(..), newBoard, perpendicularWords, mainWord)
import qualified Wordcrab.PlayResult as PlayResult
import Wordcrab.GameState (GameState(..), board, tiles, players, currentPlayer, toPreviewState)
import qualified Wordcrab.GameState as GameState
import Wordcrab.Client hiding (State)
import qualified Wordcrab.Client as Client
import qualified Wordcrab.Server as Server

main :: IO ()
main = do
  gen <- getStdGen
  let app :: App Client.State e ()
      app = App { appDraw = draw
                , appChooseCursor = Brick.showFirstCursor
                , appHandleEvent = handleEvent
                , appStartEvent = pure
                , appAttrMap = \s -> attributes
                }
      (startingRack1, (startingRack2, bag)) =
        splitAt 7 <$> splitAt 7 (Tiles.shuffleBag gen Tiles.tileset)
      gs = GameState
        { _board = Identity Board.blankBoard
        , _players = GameState.initialPlayers $ Player 0 startingRack1 :| [Player 0 startingRack2]
        , _tiles = bag
        }
      initialState = Client.State
        { Client._current = gs
        , Client._preview = Preview
          (gs & board %~ (Right . runIdentity)) Map.empty Board.blankBoard Nothing
        , Client._boardCursor = (7, 7)
        , Client._rackCursor = Nothing
        , Client._messages = []
        }
      draw :: Client.State -> [Brick.Widget ()]
      draw s = let
          boardWidget = let
            c = Brick.showCursor
                  ()
                  (Brick.Location $ s ^. boardCursor
                    & _1 *~ 4 & _1 +~ 2
                    & _2 *~ 2 & _2 +~ 1)
            b = fromRight
                  (s ^. preview . displayBoard)
                  (s ^. preview . gameState . board)
            rs = V.toList (Board.unBoard b)
            width = (4 * length rs) + 1
            w = Brick.hLimit width $ border $ Brick.vBox $ intersperse hBorder $
              fmap (Brick.vLimit 1 . Brick.hBox
                    . intersperse vBorder . V.toList
                    . fmap tileWidget
                    . Board.unRow) rs
            tileWidget :: Board.Square (Maybe Tiles.PlayedTile) -> Brick.Widget n
            tileWidget (Board.Square st mt) = let
              withAttr = case st of
                Board.Normal -> id
                Board.WordMultiplier 2 -> Brick.withAttr doubleWord
                Board.WordMultiplier 3 -> Brick.withAttr tripleWord
                Board.LetterMultiplier 2 -> Brick.withAttr doubleLetter
                Board.LetterMultiplier 3 -> Brick.withAttr tripleLetter
                _ -> id
              in withAttr $ Brick.hLimit 3 $ center $ Brick.str $ case mt of
              Nothing -> " "
              Just t -> case t of
                Tiles.PlayedBlank c -> [c]
                Tiles.PlayedLetter lt -> [Tiles.letter lt]
            in if isJust (s ^. rackCursor)
               then w
               else c w
          rackWidget' = Brick.padTop (Brick.Pad 1) $ Brick.str "Your rack:"
            <=> rackWidget
                  (s ^. preview . gameState . players . currentPlayer . rack)
                  (s ^. rackCursor)
          scoreBoxWidget :: GameState.Players -> Maybe PlayResult -> Brick.Widget n
          scoreBoxWidget ps result = Brick.vBox $
            (\(i, p) -> Brick.hLimit 61 $ Brick.strWrap $
              playerNameString i <> if p == ps ^. currentPlayer
                then scoreString p <> " (" <> moveString result <> ")"
                else scoreString p)
            <$> zip [1..] (NE.toList $ GameState.turnOrder ps)
          playerNameString i = "Player " <> show i <> ": "
          moveString :: Maybe PlayResult -> String
          moveString result = case result of
            Nothing -> "Error"
            Just pr -> "-> " <> show (pr ^. PlayResult.score) <> ": " <> showWord (NE.toList (pr ^. mainWord))
              <> ", " <> intercalate ", " (fmap showWord (pr ^. perpendicularWords))
          showWord :: [Board.TileInPlay Tiles.PlayedTile] -> String
          showWord w = let
            multiplier tip = case Board.squareType <$> tip of
              (Board.PlayedNow, Board.WordMultiplier n) -> n
              _ -> 1
            in show (product (fmap multiplier w))
            <> " x " <> concatMap showTile w
          showTile :: Board.TileInPlay Tiles.PlayedTile -> String
          showTile (when, square) = show $ case Board.squareContents square of
            Tiles.PlayedLetter lt -> (Tiles.letter lt
                                     , Board.tileMultiplier when (Board.squareType square)
                                       * fromIntegral (Tiles.score lt)
                                     )
            Tiles.PlayedBlank c -> (c, 0)
          scoreString p = show (p ^. Player.score)
          messageWidget = Brick.txt $ fromMaybe "" $ listToMaybe $ s ^. messages
        in pure $ center $ Brick.joinBorders $ Brick.vBox
          [ boardWidget
          , rackWidget'
          , scoreBoxWidget (s ^. current . players) (s ^. preview . playResult)
          , messageWidget
          ]
      backend :: Backend Identity
      backend = let
        f p d ts b = Identity $ do
            ((mw, pws, b), score) <- Board.play p d ts Tiles.tileScore b
            pure (PlayResult mw pws b score)
        in Backend
        { backendPreview = f
        , backendPlay = f
        }
      webBackend :: Backend IO
      webBackend = let
        _ :<|> _ :<|> play :<|> preview :<|> _ = client Server.wordcrabAPI
        httpManager = newManager defaultManagerSettings
        clientEnv = flip mkClientEnv (BaseUrl Http "localhost" 9432 "") <$> httpManager
        in Backend
          { backendPreview = \p d ts _ -> do
              env <- clientEnv
              result <- runClientM (preview (Server.Play p d ts)) env
              pure (first (const undefined) result)
          , backendPlay = \p d ts _ -> do
              env <- clientEnv
              result <- runClientM (play (Server.Play p d ts)) env
              pure (first (const undefined) result)
          }
      handleEvent :: Client.State -> Brick.BrickEvent n e -> Brick.EventM w (Brick.Next Client.State)
      handleEvent s = \case
        Brick.VtyEvent (EvKey (KChar 'q') (_:_)) -> Brick.halt s
        Brick.VtyEvent (EvKey (KChar c) _) -> case c of
            ' ' -> liftIO (placeOrSelectTile webBackend s) >>= Brick.continue
            c -> liftIO (updateBlank webBackend s c) >>= Brick.continue
        Brick.VtyEvent (EvKey KRight _) -> Brick.continue $ moveRight s
        Brick.VtyEvent (EvKey KLeft _) -> Brick.continue $ moveLeft s
        Brick.VtyEvent (EvKey KDown _) -> Brick.continue $ moveDown s
        Brick.VtyEvent (EvKey KUp _) -> Brick.continue $ moveUp s
        Brick.VtyEvent (EvKey KEnter _) -> do
          s <- liftIO (confirmPlay webBackend s)
          Brick.continue s
        Brick.VtyEvent (EvKey KBS _) -> do
          e <- liftIO $ sequence (pickUpTile webBackend s)
          Brick.continue $ either (message s . Text.pack) id e
        _ -> Brick.continue s
      doubleWord = Brick.attrName "doubleWord"
      tripleWord = Brick.attrName "tripleWord"
      doubleLetter = Brick.attrName "doubleLetter"
      tripleLetter = Brick.attrName "tripleLetter"
      attributes = Brick.attrMap defAttr
        [ (doubleWord, Brick.bg (rgbColor 133 182 255))
        , (tripleWord, Brick.bg (rgbColor 255 156 156))
        , (doubleLetter, Brick.bg (rgbColor 148 219 255))
        , (tripleLetter, Brick.bg (rgbColor 255 201 239))
        ]
  finalState <- defaultMain app initialState
  putStrLn "End of game"

rackWidget :: [Tiles.Tile] -> Maybe Int -> Brick.Widget ()
rackWidget ts cursor
  = maybeCursor $ Brick.hBox $ fmap (border . Brick.str . tileWidget) ts
  where tileWidget Tiles.Blank = " "
        tileWidget (Tiles.Letter lt) = pure (Tiles.letter lt)
        maybeCursor = case cursor of
                        Just c -> Brick.showCursor () (Brick.Location (1 + (3 * c), 1))
                        Nothing -> id
