{-# LANGUAGE LambdaCase #-}
module Main where

import Brick (App(..), defaultMain, attrMap, (<=>))
import qualified Brick
import Brick.Widgets.Border (border)
import Brick.Widgets.Center (center)
import Data.Bifunctor (first, second)
import Data.List.NonEmpty (NonEmpty(..))
import Graphics.Vty (defAttr, Key(..), Event(..))
import Prelude hiding (lookup)
import System.Random (getStdGen)
import Text.Read (readMaybe)

import qualified Board
import Board (Board)
import qualified Tiles
import Player (Player(..))

main :: IO ()
main = do
  gen <- getStdGen
  let app :: App ClientState e ()
      app = App { appDraw = draw
                , appChooseCursor = Brick.showFirstCursor
                , appHandleEvent = handleEvent
                , appStartEvent = pure
                , appAttrMap = \s -> attrMap defAttr []
                }
      (startingRack, bag) = splitAt 7 (Tiles.shuffleBag gen Tiles.tileset)
      gameState = GameState
        { board = Board.blankBoard
        , player = Player 0 startingRack
        , tiles = bag
        }
      initialState = ClientState
        { current = gameState
        , preview = gameState
        , cursor = (7, 7)
        }
      draw :: ClientState -> [Brick.Widget ()]
      draw s = let
          boardWidget = border $
            Brick.showCursor () (Brick.Location $ first (* 2) $ cursor s) $
              Brick.str $ Board.showBoard Tiles.showTile (board $ preview s)
          rackWidget' = Brick.padTop (Brick.Pad 1) $ Brick.str "Your rack:" <=> rackWidget (Player.rack $ player $ preview s)
          scoreWidget name score = Brick.str $ name <> " score: " <> show score
        in pure $ center $ Brick.vBox
           [ boardWidget
           , rackWidget'
           , scoreWidget "Current" (Player.score $ player $ current s)
           , scoreWidget "Projected" (Player.score $ player $ preview s)
           ]
      handleEvent :: ClientState -> Brick.BrickEvent n e -> Brick.EventM w (Brick.Next ClientState)
      handleEvent s = \case
        Brick.VtyEvent (EvKey (KChar c) _) -> case readMaybe (pure c) :: Maybe Integer of
          Just i -> Brick.continue s
          Nothing -> case c of
            'q' -> Brick.halt s
            c -> Brick.continue (updatePreview (Tiles.PlayedLetter $ Tiles.LetterTile c 0) s)
        Brick.VtyEvent (EvKey KRight _) ->
          Brick.continue $ moveRight s
        Brick.VtyEvent (EvKey KLeft _) ->
          Brick.continue $ moveLeft s
        Brick.VtyEvent (EvKey KDown _) ->
          Brick.continue $ moveDown s
        Brick.VtyEvent (EvKey KUp _) ->
          Brick.continue $ moveUp s
        _ -> Brick.continue s
  finalState <- defaultMain app initialState
  putStrLn "End of game"

data ClientState = ClientState
  { current :: GameState
  , preview :: GameState
  , cursor :: (Int, Int)
  }

data GameState = GameState
  { board :: Board Tiles.PlayedTile
  , player :: Player
  , tiles :: [Tiles.Tile]
  }

moveRight :: ClientState -> ClientState
moveRight cs = let
  right = first ((`mod` 15) . (+ 1)) (cursor cs)
  in cs { cursor = right }

moveLeft :: ClientState -> ClientState
moveLeft cs = let
  left = first ((`mod` 15) . (subtract 1)) (cursor cs)
  in cs { cursor = left }

moveUp :: ClientState -> ClientState
moveUp cs = let
  up = second ((`mod` 15) . (subtract 1)) (cursor cs)
  in cs { cursor = up }

moveDown :: ClientState -> ClientState
moveDown cs = let
  down = second ((`mod` 15) . (+ 1)) (cursor cs)
  in cs { cursor = down }

toBoardPosition :: (Int, Int) -> Board.Position
toBoardPosition (x, y) = Board.Position x y

updatePreview :: Tiles.PlayedTile -> ClientState -> ClientState
updatePreview t cs = let
  m =
    Board.play (toBoardPosition (cursor cs))
      Board.Horizontal
      (t :| [])
      Tiles.tileScore
      (board $ current cs)
  in flip (maybe cs) m
     (\((b, _, _), s) ->
         cs { preview = (preview cs) {
                board = b,
                player = (player $ preview cs) {
                    score = (score $ player $ preview cs) + s
                    }
                }
            })

rackWidget :: [Tiles.Tile] -> Brick.Widget ()
rackWidget ts = Brick.hBox $ fmap (border . Brick.str . tileWidget) ts
  where tileWidget Tiles.Blank = " "
        tileWidget (Tiles.Letter lt) = pure (Tiles.letter lt)
