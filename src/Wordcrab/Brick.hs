{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Wordcrab.Brick where

import Brick (App(..), defaultMain, attrMap, (<=>))
import qualified Brick
import Brick.Widgets.Border (border, hBorder, vBorder)
import Brick.Widgets.Center (center)
import Control.Lens ((^.), (*~), _1, _2, to, (.~), (+~), (%~), (?~))
import Data.Bifunctor (first, second)
import Data.Either (fromRight)
import Data.Function ((&))
import Data.Functor.Identity (Identity(..))
import Data.List (intersperse)
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
                , appAttrMap = \s -> attributes
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
          = PreviewState (gs & board %~ (Right . runIdentity)) Map.empty Board.blankBoard
        , _boardCursor = (7, 7)
        , _rackCursor = Nothing
        , _messages = []
        }
      draw :: ClientState -> [Brick.Widget ()]
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
                  (s ^. preview . gameState . player . rack)
                  (s ^. rackCursor)
          scoreWidget name score = Brick.str $ name <> " score: " <> show score
          messageWidget = Brick.txt $ fromMaybe "" $ listToMaybe $ s ^. messages
        in pure $ center $ Brick.joinBorders $ Brick.vBox
          [ boardWidget
          , rackWidget'
          , scoreWidget "Current" (s ^. current . player . score)
          , scoreWidget "Projected" (s ^. preview . gameState . player . score)
          , messageWidget
          ]
      handleEvent :: ClientState -> Brick.BrickEvent n e -> Brick.EventM w (Brick.Next ClientState)
      handleEvent s = \case
        Brick.VtyEvent (EvKey (KChar 'q') (_:_)) -> Brick.halt s
        Brick.VtyEvent (EvKey (KChar c) _) -> case c of
            ' ' -> Brick.continue $ placeOrSelectTile s
            c -> Brick.continue $ updateBlank s c
        Brick.VtyEvent (EvKey KRight _) -> Brick.continue $ moveRight s
        Brick.VtyEvent (EvKey KLeft _) -> Brick.continue $ moveLeft s
        Brick.VtyEvent (EvKey KDown _) -> Brick.continue $ moveDown s
        Brick.VtyEvent (EvKey KUp _) -> Brick.continue $ moveUp s
        Brick.VtyEvent (EvKey KEnter _) -> Brick.continue $ confirmPlay s
        Brick.VtyEvent (EvKey KBS _) -> Brick.continue $ either (message s . Text.pack) id (pickUpTile s)
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

confirmPlay :: ClientState -> ClientState
confirmPlay cs = let
  newBoard = cs ^. preview . gameState . board
  newScore = cs ^. preview . gameState . player . score
  remainingTiles = cs ^. preview . gameState . player . rack
  needed = 7 - length remainingTiles
  (newTiles, newBag) = splitAt needed (cs ^. preview . gameState . tiles)
  in case newBoard of
    Right b -> cs
               & current . player . score .~ newScore
               & current . board .~ Identity b
               & current . player . rack .~ (remainingTiles <> newTiles)
               & current . tiles .~ newBag
               & preview . placed .~ Map.empty
               & \cs' -> cs' & preview . gameState .~ toPreviewState (cs' ^. current)
    Left e -> message cs $ "Can’t play: invalid move (" <> Text.pack (show e) <> ")"

message :: ClientState -> Text -> ClientState
message cs m = cs & messages %~ (m :)

placeOrSelectTile :: ClientState -> ClientState
placeOrSelectTile cs =
  if cursorOnBoard
  then if spaceFree
    then cs & rackCursor ?~ 0
    else message cs "Can't place on another tile"
  else fromRight cs $ placeTile cs
  where
    cursorOnBoard = cs ^. rackCursor . to isNothing
    spaceFree = isNothing cell
    (x, y) = cs ^. boardCursor
    vector = Board.unBoard (cs ^. preview . displayBoard)
    row = Board.unRow (vector V.! y)
    cell = Board.squareContents $ row V.! x

updateBlank :: ClientState -> Char -> ClientState
updateBlank s c = let
  cursor = s ^. boardCursor
  target = Map.lookup cursor (s ^. preview . placed)
  in case (s ^. rackCursor, target) of
    (Nothing, Just (Tiles.PlayedBlank _)) ->
      snd $ updatePreview $
        s & preview . placed %~ Map.adjust (const $ Tiles.PlayedBlank c) cursor
    _ -> message s "Can only add a letter to a blank tile you've placed this turn"

updatePreview :: ClientState -> (Maybe OrganiseError, ClientState)
updatePreview cs = runIdentity $ do
  cs <- pure $ cs
          & preview . gameState . board .~ (cs ^. current . board . to runIdentity . to pure)
          & preview . gameState . player . score .~ (cs ^. current . player . score)
  let ot = organiseTiles cs
  cs <- pure $ case ot of
    Left NoTiles -> cs
    Left InconsistentDirection -> cs
    Right (p, d, ts) -> do
      let m = Board.play p
                d
                ts
                Tiles.tileScore
                (runIdentity $ cs ^. current . board)
      case m of
        Right ((b, _, _), s) ->
          cs & preview . gameState . board .~ Right b
             & preview . gameState . player . score .~ (s + (cs ^. current . player . score))
        Left e -> cs & preview . gameState . board .~ Left e
  let db = foldr (\(xy, t) b -> updateBoard xy t b)
             (cs ^. current . board . to runIdentity)
             (Map.toList $ cs ^. preview . placed)
  pure (either Just (const Nothing) ot, cs & preview . displayBoard .~ db)

-- | TODO: Prevent playing over a tile you just played
placeTile :: ClientState -> Either PlaceError ClientState
placeTile cs = do
  i <- note NoCursor $ cs ^. rackCursor
  let playedTile = case (cs ^. preview . gameState . player . rack) !! i of
        Tiles.Letter lt -> Tiles.PlayedLetter lt
        Tiles.Blank -> Tiles.PlayedBlank ' '
  cs <- pure $ cs & (rackCursor .~ Nothing)
    & preview . placed %~ Map.insert (cs ^. boardCursor) playedTile
    & preview . gameState . player . rack %~ remove i
  case updatePreview cs of
    (Nothing, cs) -> pure cs
    (Just e, cs) -> pure $ cs & flip message ("can't play: invalid move (" <> Text.pack (show e) <> ")")

pickUpTile :: ClientState -> Either String ClientState
pickUpTile cs = do
  _ <- maybe (Right ()) (const $ Left "Can’t delete from rack") (cs ^. rackCursor)
  let m = Map.lookup (cs ^. boardCursor) (cs ^. preview . placed)
  case m of
    Nothing -> Left "Can’t delete a tile you didn’t place this turn"
    Just t -> pure $
      snd $
        updatePreview $
          cs & preview . placed %~ Map.delete (cs ^. boardCursor)
             & preview . gameState . player . rack %~ (Tiles.unplay t :)

note :: a -> Maybe b -> Either a b
note a = maybe (Left a) Right

data PlaceError = Organise OrganiseError | NoCursor

organiseTiles :: ClientState -> Either OrganiseError (Board.Position, Board.Direction, NonEmpty Tiles.PlayedTile)
organiseTiles cs = case Map.toList $ cs ^. preview . placed of
  [] -> Left NoTiles
  t@((x, y), _) : ts -> let
    sorted = NE.sortWith (\(coords, _) -> f coords) (t :| ts)
    horizontal = all ((== y) . snd . fst) ts
    vertical = all ((== x) . fst . fst) ts
    f = case (horizontal, vertical) of
      (True, False) -> fst
      _ -> snd
    direction = case (horizontal, vertical) of
      (True, True) -> Right Board.Horizontal
      (True, False) -> Right Board.Horizontal
      (False, True) -> Right Board.Vertical
      (False, False) -> Left InconsistentDirection
    position = uncurry Board.Position $ fst $ NE.head sorted
    in do
      d <- direction
      pure (position, d, fmap snd sorted)

data OrganiseError = NoTiles | InconsistentDirection deriving Show

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

unupdateBoard
  :: (Int, Int)
  -> Board.Board Tiles.PlayedTile
  -> Board.Board Tiles.PlayedTile
unupdateBoard (x, y) b = let
  v = Board.unBoard b
  r = Board.unRow $ v V.! y
  s = r V.! x
  r' = r V.// [(x, s { Board.squareContents = Nothing })]
  in Board.Board $ v V.// [(y, Board.Row r')]

rackWidget :: [Tiles.Tile] -> Maybe Int -> Brick.Widget ()
rackWidget ts cursor
  = maybeCursor $ Brick.hBox $ fmap (border . Brick.str . tileWidget) ts
  where tileWidget Tiles.Blank = " "
        tileWidget (Tiles.Letter lt) = pure (Tiles.letter lt)
        maybeCursor = case cursor of
                        Just c -> Brick.showCursor () (Brick.Location (1 + (3 * c), 1))
                        Nothing -> id
