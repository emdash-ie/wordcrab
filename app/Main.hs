{-# LANGUAGE LambdaCase #-}
module Main where

import Brick (App(..), defaultMain, attrMap, (<=>))
import qualified Brick
import Brick.Widgets.Border (border)
import Brick.Widgets.Center (center)
import Data.Bifunctor (first, second)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Maybe (isJust, isNothing)
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
      gs = GameState
        { board = Board.blankBoard
        , player = Player 0 startingRack
        , tiles = bag
        }
      initialState = ClientState
        { current = gs
        , preview = PreviewState gs []
        , boardCursor = (7, 7)
        , rackCursor = Nothing
        }
      draw :: ClientState -> [Brick.Widget ()]
      draw s = let
          boardWidget = let
            b = Brick.str $ Board.showBoard Tiles.showTile (board $ gameState $ preview s)
            c = Brick.showCursor () (Brick.Location $ first (* 2) $ boardCursor s)
            in border $ if isJust (rackCursor s)
                        then b
                        else c b
          rackWidget' = Brick.padTop (Brick.Pad 1) $ Brick.str "Your rack:" <=> rackWidget (Player.rack $ player $ gameState $ preview s) (rackCursor s)
          scoreWidget name score = Brick.str $ name <> " score: " <> show score
        in pure $ center $ Brick.vBox
           [ boardWidget
           , rackWidget'
           , scoreWidget "Current" (Player.score $ player $ current s)
           , scoreWidget "Projected" (Player.score $ player $ gameState $ preview s)
           ]
      handleEvent :: ClientState -> Brick.BrickEvent n e -> Brick.EventM w (Brick.Next ClientState)
      handleEvent s = \case
        Brick.VtyEvent (EvKey (KChar c) _) -> case readMaybe (pure c) :: Maybe Integer of
          Just i -> Brick.continue s
          Nothing -> case c of
            'q' -> Brick.halt s
            ' ' -> Brick.continue $ placeOrSelectTile s
            c -> Brick.continue (updatePreview (Tiles.PlayedLetter $ Tiles.LetterTile c 0) s)
        Brick.VtyEvent (EvKey KRight _) -> Brick.continue $ moveRight s
        Brick.VtyEvent (EvKey KLeft _) -> Brick.continue $ moveLeft s
        Brick.VtyEvent (EvKey KDown _) -> Brick.continue $ moveDown s
        Brick.VtyEvent (EvKey KUp _) -> Brick.continue $ moveUp s
        _ -> Brick.continue s
  finalState <- defaultMain app initialState
  putStrLn "End of game"

data ClientState = ClientState
  { current :: GameState
  , preview :: PreviewState
  , boardCursor :: (Int, Int)
  , rackCursor :: Maybe Int
  }

data GameState = GameState
  { board :: Board Tiles.PlayedTile
  , player :: Player
  , tiles :: [Tiles.Tile]
  }

data PreviewState = PreviewState
  { gameState :: GameState
  , placed :: [(Tiles.PlayedTile, (Int, Int))]
  }

placeOrSelectTile :: ClientState -> ClientState
placeOrSelectTile cs = if selecting
  then cs { rackCursor = Just 0 }
  else placeTile cs
  where
    selecting = isNothing (rackCursor cs)

placeTile :: ClientState -> ClientState
placeTile cs = cs { rackCursor = Nothing }

move ::
  ((Int -> Int) -> (Int, Int) -> (Int, Int)) ->
  (Int -> Int) ->
  ClientState ->
  ClientState
move f g cs = let
  boardMove = f ((`mod` 15) . g) (boardCursor cs)
  rackMove = fmap ((`mod` 7) . g) (rackCursor cs)
  in if isJust (rackCursor cs)
     then cs { rackCursor = rackMove }
     else cs { boardCursor = boardMove }

moveRight :: ClientState -> ClientState
moveRight = move first (+ 1)

moveLeft :: ClientState -> ClientState
moveLeft = move first (subtract 1)

moveUp :: ClientState -> ClientState
moveUp = move second (subtract 1)

moveDown :: ClientState -> ClientState
moveDown = move second (+ 1)

toBoardPosition :: (Int, Int) -> Board.Position
toBoardPosition (x, y) = Board.Position x y

updatePreview :: Tiles.PlayedTile -> ClientState -> ClientState
updatePreview t cs = let
  m =
    Board.play (toBoardPosition (boardCursor cs))
      Board.Horizontal
      (t :| [])
      Tiles.tileScore
      (board $ current cs)
  in flip (maybe cs) m
     (\((b, _, _), s) ->
         cs { preview = (preview cs) {
                gameState = (gameState $ preview cs) {
                  board = b,
                  player = (player $ gameState $ preview cs) {
                      score = (score $ player $ gameState $ preview cs) + s
                      }
                }
                                                     }
            })

rackWidget :: [Tiles.Tile] -> Maybe Int -> Brick.Widget ()
rackWidget ts cursor
  = maybeCursor $ Brick.hBox $ fmap (border . Brick.str . tileWidget) ts
  where tileWidget Tiles.Blank = " "
        tileWidget (Tiles.Letter lt) = pure (Tiles.letter lt)
        maybeCursor = case cursor of
                        Just c -> Brick.showCursor () (Brick.Location (1 + (3 * c), 1))
                        Nothing -> id
