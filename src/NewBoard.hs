module NewBoard where

import Control.Category ((>>>))
import Control.Monad (guard)
import Data.Bifunctor (first, second)
import Data.Bool (bool)
import Data.Foldable (fold)
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.List (intersperse)
import qualified Data.List.NonEmpty as NE
import Data.List.NonEmpty (NonEmpty(..))
import Data.Maybe (fromJust, isJust, catMaybes)
import qualified Data.Vector as V
import Data.Vector (Vector)
import Prelude hiding (lookup)

import qualified Board
import qualified Tiles

newtype Board = Board { unBoard :: Vector Row }
newtype Row = Row { unRow :: Vector (Square (Maybe Tiles.PlayedTile)) }

data SquareType = Normal | WordMultiplier Int | LetterMultiplier Int deriving Show
data Square a = Square { squareType :: SquareType, squareContents :: a } deriving Show

isWordMultiplier :: SquareType -> Bool
isWordMultiplier (WordMultiplier _) = True
isWordMultiplier _ = False

totalWordMultiplier :: [SquareType] -> Int
totalWordMultiplier = filter isWordMultiplier
  >>> fmap (\(WordMultiplier n) -> n)
  >>> product

newtype ValidPosition = ValidPosition { unwrapPosition :: Board.Position } deriving (Eq, Show)

validatePosition :: Board.Position -> Board -> Maybe ValidPosition
validatePosition p (Board rs) = let
  validY = Board.positionY p < fromIntegral (length rs) && Board.positionY p > 0
  validX = case rs V.!? 0 of
    Nothing -> False
    Just (Row ts) -> Board.positionX p < fromIntegral (length ts) && Board.positionX p > 0
  in bool Nothing (Just $ ValidPosition p) (validY && validX)

blankBoard :: Board
blankBoard = let
  squares = (fmap . fmap) (\t -> Square t Nothing) squareTypes
  in Board $ V.fromList $ fmap (V.fromList >>> Row) squares

squareTypes :: [[SquareType]]
squareTypes = let
  n = Normal
  dl = LetterMultiplier 2
  tl = LetterMultiplier 3
  dw = WordMultiplier 2
  tw = WordMultiplier 3
  upperLeft = [[tw, n, n, dl, n, n, n, tw], [n, dw, n, n, n, tl, n, n],
               [n, n, dw, n, n, n, dl, n],  [dl, n, n, dw, n, n, n, dl],
               [n, n, n, n, dw, n, n, n],   [n, tl, n, n, n, tl, n, n],
               [n, n, dl, n, n, n, dl, n],  [tw, n, n, dl, n, n, n, dw]]
  reflect xs = xs <> tail (reverse xs)
  in reflect (fmap reflect upperLeft)

lookup :: Board -> ValidPosition -> Square (Maybe Tiles.PlayedTile)
lookup (Board rs) (ValidPosition p) = unRow (rs V.! Board.positionY p) V.! Board.positionX p

update :: Board -> ValidPosition -> Tiles.PlayedTile -> Maybe Board
update b p t = case squareContents $ lookup b p of
  Just _ -> Nothing
  Nothing -> Just $ write b p t

write :: Board -> ValidPosition -> Tiles.PlayedTile -> Board
write (Board rs) (ValidPosition p) t = let
  Row oldRow = rs V.! Board.positionY p
  s = oldRow V.! Board.positionX p
  newRow = Row $ oldRow V.// [(Board.positionX p, Square (squareType s) (Just t))]
  in Board (rs V.// [(Board.positionY p, newRow)])

writeSeveral :: Foldable a => a (ValidPosition, Tiles.PlayedTile) -> Board -> Board
writeSeveral xs b = foldr (\(p, t) b' -> write b' p t) b xs

playIndices ::
  Board.Position ->
  Board.Direction ->
  NonEmpty Tiles.PlayedTile ->
  Board ->
  Maybe (NonEmpty (ValidPosition, Square Tiles.PlayedTile))
playIndices p d ts b = do
  vp <- validatePosition p b
  case lookup b vp of
    Square _ (Just _) -> playIndices (Board.forward d p) d ts b
    Square st Nothing -> case NE.uncons ts of
        (t, Nothing) -> Just $ (vp, Square st t) :| []
        (t, Just ts') -> NE.cons (vp, Square st t)
                           <$> playIndices (Board.forward d p) d ts' b

bordersWord ::
  NonEmpty ValidPosition ->
  Board ->
  Bool
bordersWord ps b = let
  hasNeighbours p = any (squareContents >>> isJust) (neighbours p b)
  in any hasNeighbours ps

neighbours ::
  ValidPosition ->
  Board ->
  NonEmpty (Square (Maybe Tiles.PlayedTile))
neighbours (ValidPosition p) b = let
  f p' = validatePosition p' b <&> lookup b
  above = f (Board.backward Board.Vertical p)
  below = f (Board.forward Board.Vertical p)
  right = f (Board.backward Board.Horizontal p)
  left = f (Board.forward Board.Horizontal p)
  -- | fromList: Every space has at least two valid neighbours
  in NE.fromList (catMaybes [above, below, right, left])

wordAt ::
  ValidPosition ->
  Board.Direction ->
  Board ->
  [(ValidPosition, Square Tiles.PlayedTile)]
wordAt p d b = wordFrom (startOfWord p d b) d b

startOfWord ::
  ValidPosition ->
  Board.Direction ->
  Board ->
  ValidPosition
startOfWord vp@(ValidPosition p) d b =
  case validatePosition (Board.backward d p) b of
    Nothing -> vp
    Just p' -> case lookup b p' of
      Square _ Nothing -> vp
      Square _ (Just _) -> startOfWord p' d b

wordFrom ::
  ValidPosition ->
  Board.Direction ->
  Board ->
  [(ValidPosition, Square Tiles.PlayedTile)]
wordFrom vp@(ValidPosition p) d b = case lookup b vp of
  Square _ Nothing -> []
  Square st (Just t) -> case validatePosition (Board.forward d p) b of
    Nothing -> [(vp, Square st t)]
    Just vp' -> (vp, Square st t) : wordFrom vp' d b

type Play = (Board, NonEmpty TileInPlay, [[TileInPlay]])
type TileInPlay = (PlayedWhen, Square Tiles.PlayedTile)
data PlayedWhen = PlayedNow | PlayedEarlier deriving (Show, Eq)

play ::
  Board.Position ->
  Board.Direction ->
  NonEmpty Tiles.PlayedTile ->
  Board ->
  Maybe (Play, Integer)
play p d ts b = do
  indices <- playIndices p d ts b
  let playedIndices = fmap fst indices
  guard (bordersWord playedIndices b
         || elem (Board.Position 7 7) (fmap (fst >>> unwrapPosition) indices))
  let b' = writeSeveral (NE.map (second (\(Square _ x) -> x)) indices) b
  let mainWord = NE.fromList $ wordAt (NE.head playedIndices) d b'
  let perpWords = NE.filter ((> 1) . length) $ fmap (\i -> wordAt i (succ d) b') playedIndices
  let active = first (\i  -> bool PlayedEarlier PlayedNow (i `elem` playedIndices))
  let play = (b', fmap active mainWord, (fmap . fmap) active perpWords)
  pure (play, score play)

score :: Play -> Integer
score (_, mainWord, perpWords) =
  scoreWord (NE.toList mainWord) + sum (fmap scoreWord perpWords)

scoreWord :: [TileInPlay] -> Integer
scoreWord ts = let
  activeMultipliers = fmap (snd . second squareType)
                $ filter (fst >>> (==) PlayedNow) ts
  wordMultiplier = fromIntegral $ totalWordMultiplier activeMultipliers
  letterScore (PlayedNow, Square (LetterMultiplier n) t) = fromIntegral n * Tiles.tileScore t
  letterScore (_, Square _ t) = Tiles.tileScore t
  in wordMultiplier * sum (fmap letterScore ts)

testBoard :: IO ()
testBoard = let
  display ((b, mw, pws), s) = putStrLn (showBoard b)
    >> putStrLn "Main word:" >> traverse (showTile >>> putStrLn) mw
    >> putStrLn "Perp words:" >> traverse (fmap showTile >>> traverse putStrLn) pws
    >> putStrLn ("Score: " <> show s)
  showTile :: TileInPlay -> String
  showTile (when, square) = let
    active = case when of PlayedNow -> " (*)"
                          PlayedEarlier -> ""
    in "- " <> show (squareType square) <> active <> ": "
       <> show (squareContents square)
  b = blankBoard
  t1@((b1, mw1, pw1), s1) = fromJust $
    play (Board.Position 7 7) Board.Horizontal
      (fmap Tiles.PlayedLetter (Tiles.a :| [Tiles.b])) b
  t2@((b2, mw2, pw2), s2) = fromJust $
    play (Board.Position 6 7) Board.Horizontal (fmap Tiles.PlayedLetter (Tiles.t :| [Tiles.l, Tiles.e])) b1
  t3@((b3, mw3, pw3), s3) = fromJust $
    play (Board.Position 9 4) Board.Vertical (fmap Tiles.PlayedLetter (Tiles.h :| [Tiles.e, Tiles.l])) b2
  t4@((b4, mw4, pw4), s4) = fromJust $
    play (Board.Position 6 8) Board.Vertical (fmap Tiles.PlayedLetter (Tiles.h :| [Tiles.i, Tiles.n, Tiles.k])) b3
  t5@((b5, mw5, pw5), s5) = fromJust $
    play (Board.Position 9 8) Board.Horizontal (fmap Tiles.PlayedLetter (Tiles.o :| [Tiles.x, Tiles.e, Tiles.n])) b4
  fullSequence = putStrLn "1:" >> display t1 >> getLine
                 >> putStrLn "2:" >> display t2 >> getLine
                 >> putStrLn "3:" >> display t3 >> getLine
                 >> putStrLn "4:" >> display t4 >> getLine
                 >> putStrLn "5:" >> display t5
  in fullSequence

showBoard :: Board -> String
showBoard (Board rs) = let
  inner = "\n" <> replicate 20 '-' <> "\n"
  in fold . intersperse "\n" . fmap showRow $ V.toList rs

showRow :: Row -> String
showRow (Row ts) = fmap showCell ts & V.toList & intersperse "|" & fold

showCell :: Square (Maybe Tiles.PlayedTile) -> String
showCell (Square _ Nothing) = "_"
showCell (Square _ (Just (Tiles.PlayedBlank c))) = [c]
showCell (Square _ (Just (Tiles.PlayedLetter lt))) = [Tiles.letter lt]

