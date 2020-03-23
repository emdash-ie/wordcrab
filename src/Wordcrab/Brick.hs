{-# LANGUAGE LambdaCase #-}
module Wordcrab.Brick where

import Brick (App(..), defaultMain, attrMap, (<=>))
import qualified Brick
import Brick.Widgets.Border (border)
import Brick.Widgets.Center (center)
import Control.Lens ((^.), (*~), _1, to, (.~), (+~))
import Data.Bifunctor (first, second)
import Data.Function ((&))
import Data.List.NonEmpty (NonEmpty(..))
import Data.Maybe (isJust, isNothing)
import Graphics.Vty (defAttr, Key(..), Event(..))
import Prelude hiding (lookup)
import System.Random (getStdGen)
import Text.Read (readMaybe)

import qualified Wordcrab.Board as Board
import qualified Wordcrab.Tiles as Tiles
import Wordcrab.Player (Player(..), score, rack)
import Wordcrab.Brick.ClientState

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
        { _board = Board.blankBoard
        , _player = Player 0 startingRack
        , _tiles = bag
        }
      initialState = ClientState
        { _current = gs
        , _preview = PreviewState gs []
        , _boardCursor = (7, 7)
        , _rackCursor = Nothing
        }
      draw :: ClientState -> [Brick.Widget ()]
      draw s = let
          boardWidget = let
            b = Brick.str $ Board.showBoard Tiles.showTile (s ^. preview . gameState . board)
            c = Brick.showCursor () (Brick.Location $ s ^. boardCursor & _1 *~ 2)
            in border $ if isJust (s ^. rackCursor)
                        then b
                        else c b
          rackWidget' = Brick.padTop (Brick.Pad 1) $ Brick.str "Your rack:"
            <=> rackWidget
                  (s ^. preview . gameState . player . rack)
                  (s ^. rackCursor)
          scoreWidget name score = Brick.str $ name <> " score: " <> show score
        in pure $ center $ Brick.vBox
           [ boardWidget
           , rackWidget'
           , scoreWidget "Current" (s ^. current . player . score)
           , scoreWidget "Projected" (s ^. preview . gameState . player . score)
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

placeOrSelectTile :: ClientState -> ClientState
placeOrSelectTile cs = if selecting
  then cs & rackCursor .~ Just 0
  else placeTile cs
  where
    selecting = cs ^. rackCursor . to isNothing

placeTile :: ClientState -> ClientState
placeTile = rackCursor .~ Nothing

move ::
  ((Int -> Int) -> (Int, Int) -> (Int, Int)) ->
  (Int -> Int) ->
  ClientState ->
  ClientState
move f g cs = let
  boardMove = f ((`mod` 15) . g) (cs ^. boardCursor)
  rackMove = fmap ((`mod` 7) . g) (cs ^. rackCursor)
  in if isJust (cs ^. rackCursor)
     then rackCursor .~ rackMove $ cs
     else boardCursor .~ boardMove $ cs

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
    Board.play (toBoardPosition (cs ^. boardCursor))
      Board.Horizontal
      (t :| [])
      Tiles.tileScore
      (cs ^. current . board)
  in flip (maybe cs) m
     (\((b, _, _), s) ->
         cs & preview . gameState . board .~ b
            & preview . gameState . player . score +~ s)

rackWidget :: [Tiles.Tile] -> Maybe Int -> Brick.Widget ()
rackWidget ts cursor
  = maybeCursor $ Brick.hBox $ fmap (border . Brick.str . tileWidget) ts
  where tileWidget Tiles.Blank = " "
        tileWidget (Tiles.Letter lt) = pure (Tiles.letter lt)
        maybeCursor = case cursor of
                        Just c -> Brick.showCursor () (Brick.Location (1 + (3 * c), 1))
                        Nothing -> id
