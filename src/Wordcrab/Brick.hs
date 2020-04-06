{-# LANGUAGE LambdaCase #-}
module Wordcrab.Brick where

import Brick (App(..), defaultMain, attrMap, (<=>))
import qualified Brick
import Brick.Widgets.Border (border)
import Brick.Widgets.Center (center)
import Control.Lens ((^.), (*~), _1, to, (.~), (+~), (%~), (?~))
import Data.Bifunctor (first, second)
import Data.Either (fromRight, isLeft)
import Data.Function ((&))
import Data.Functor.Identity (Identity(..))
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import Data.Maybe (isJust, isNothing, fromJust)
import qualified Data.Vector as V
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
        { _board = Identity Board.blankBoard
        , _player = Player 0 startingRack
        , _tiles = bag
        }
      initialState = ClientState
        { _current = gs
        , _preview
          = PreviewState (gs & board %~ (Right . runIdentity)) [] Board.blankBoard
        , _boardCursor = (7, 7)
        , _rackCursor = Nothing
        }
      draw :: ClientState -> [Brick.Widget ()]
      draw s = let
          boardWidget = let
            b = Brick.str $ case s ^. preview . gameState . board of
              Left e -> "Error: " <> show e
              Right b -> Board.showBoard Tiles.showTile b
            c = Brick.showCursor () (Brick.Location $ s ^. boardCursor & _1 *~ 2)
            in border $ if isJust (s ^. rackCursor)
                        then b
                        else c b
          rackWidget' = Brick.padTop (Brick.Pad 1) $ Brick.str "Your rack:"
            <=> rackWidget
                  (s ^. preview . gameState . player . rack)
                  (s ^. rackCursor)
          scoreWidget name score = Brick.str $ name <> " score: " <> show score
          validationWidget = Brick.str $ if isLeft (s ^. preview . gameState . board)
                                         then "(Invalid move)"
                                         else ""
        in pure $ center $ Brick.vBox
           [ boardWidget
           , rackWidget'
           , scoreWidget "Current" (s ^. current . player . score)
           , scoreWidget "Projected" (s ^. preview . gameState . player . score)
           , validationWidget
           ]
      handleEvent :: ClientState -> Brick.BrickEvent n e -> Brick.EventM w (Brick.Next ClientState)
      handleEvent s = \case
        Brick.VtyEvent (EvKey (KChar c) _) -> case readMaybe (pure c) :: Maybe Integer of
          Just i -> Brick.continue s
          Nothing -> case c of
            'q' -> Brick.halt s
            ' ' -> Brick.continue $ placeOrSelectTile s
            _ -> Brick.continue s
        Brick.VtyEvent (EvKey KRight _) -> Brick.continue $ moveRight s
        Brick.VtyEvent (EvKey KLeft _) -> Brick.continue $ moveLeft s
        Brick.VtyEvent (EvKey KDown _) -> Brick.continue $ moveDown s
        Brick.VtyEvent (EvKey KUp _) -> Brick.continue $ moveUp s
        _ -> Brick.continue s
  finalState <- defaultMain app initialState
  putStrLn "End of game"

placeOrSelectTile :: ClientState -> ClientState
placeOrSelectTile cs = if selecting
  then cs & rackCursor ?~ 0
  else fromRight cs $ placeTile cs
  where
    selecting = cs ^. rackCursor . to isNothing

placeTile :: ClientState -> Either PlaceError ClientState
placeTile cs = do
  i <- note NoCursor $ cs ^. rackCursor
  let playedTile = case (cs ^. preview . gameState . player . rack) !! i of
        Tiles.Letter lt -> Tiles.PlayedLetter lt
        Tiles.Blank -> Tiles.PlayedBlank 'N' -- TODO: prompt player for letter
  cs <- pure $ cs & (rackCursor .~ Nothing)
    & preview . placed %~ (:) (playedTile, cs ^. boardCursor)
    & preview . gameState . player . rack %~ remove i
  (p, d, ts) <- first Organise $ organiseTiles cs
  let m = Board.play p
        d
        ts
        Tiles.tileScore
        (runIdentity $ cs ^. current . board)
  -- cs <- pure $ cs & preview . displayBoard %~ updateBoard (cs ^. boardCursor) playedTile
  pure $ case m of
    Right ((b, _, _), s) -> cs & preview . gameState . board .~ Right b
                           & preview . gameState . player . score .~ (s + (cs ^. current . player . score))
    Left e -> cs & preview . gameState . board .~ Left e

note :: a -> Maybe b -> Either a b
note a = maybe (Left a) Right

data PlaceError = Organise OrganiseError | NoCursor

organiseTiles :: ClientState -> Either OrganiseError (Board.Position, Board.Direction, NonEmpty Tiles.PlayedTile)
organiseTiles cs = case cs ^. preview . placed of
  [] -> Left NoTiles
  t@(_, (x, y)) : ts -> let
    sorted = NE.sortWith (\(_, coords) -> f coords) (t :| ts)
    horizontal = all ((== y) . snd . snd) ts
    vertical = all ((== x) . fst . snd) ts
    f = case (horizontal, vertical) of
      (True, False) -> fst
      _ -> snd
    direction = case (horizontal, vertical) of
      (True, True) -> Right Board.Horizontal
      (True, False) -> Right Board.Horizontal
      (False, True) -> Right Board.Vertical
      (False, False) -> Left InconsistentDirection
    position = uncurry Board.Position $ snd $ NE.head sorted
    in do
      d <- direction
      pure (position, d, fmap fst sorted)

data OrganiseError = NoTiles | InconsistentDirection

remove :: Int -> [a] -> [a]
remove i xs = take i xs <> drop (i + 1) xs

move ::
  ((Int -> Int) -> (Int, Int) -> (Int, Int)) ->
  (Int -> Int) ->
  ClientState ->
  ClientState
move f g cs = let
  boardMove = f ((`mod` 15) . g) (cs ^. boardCursor)
  rackMove = fmap ((`mod` (length $ cs ^. preview . gameState . player . rack)) . g)
             (cs ^. rackCursor)
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

updateBoard ::
  (Int, Int) ->
  Tiles.PlayedTile ->
  Board.Board Tiles.PlayedTile ->
  Board.Board Tiles.PlayedTile
updateBoard (x, y) t b = let
  v = Board.unBoard b
  r = Board.unRow $ v V.! y
  s = r V.! x
  r' = r V.// [(x, s { Board.squareContents = Just t })]
  in Board.Board $ v V.// [(y, Board.Row r')]

rackWidget :: [Tiles.Tile] -> Maybe Int -> Brick.Widget ()
rackWidget ts cursor
  = maybeCursor $ Brick.hBox $ fmap (border . Brick.str . tileWidget) ts
  where tileWidget Tiles.Blank = " "
        tileWidget (Tiles.Letter lt) = pure (Tiles.letter lt)
        maybeCursor = case cursor of
                        Just c -> Brick.showCursor () (Brick.Location (1 + (3 * c), 1))
                        Nothing -> id
