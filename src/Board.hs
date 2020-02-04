module Board where

import Data.Bool (bool)
import Control.Category ((>>>))
import Control.Monad ((>=>))
import Data.Foldable (fold)
import Data.List (intersperse)
import Data.Maybe (listToMaybe)

import qualified Tiles

newtype Board = Board [Row]
type Row = [Maybe Tiles.PlayedTile]
type Column = [Maybe Tiles.PlayedTile]

testBoard :: IO (Maybe ())
testBoard = let
  display = traverse (putStrLn . showBoard)
  b = blankBoard 10 10
  moves = update' (Position 2 5) Horizontal (Tiles.blanks "ab")
          >=> update' (Position 1 5) Horizontal (Tiles.blanks "tle")
          >=> update' (Position 4 2) Vertical (Tiles.blanks "helo")
  moves' = update'' (Position 2 5) Horizontal (Tiles.blanks "ab")
           >=> update'' (Position 1 5) Horizontal (Tiles.blanks "tle")
           >=> update'' (Position 4 2) Vertical (Tiles.blanks "helo")
  moves'' = moves' >=> update'' (Position 1 6) Vertical (Tiles.blanks "hinking")
  in putStrLn "1:"
     >> display (moves b)
     >> putStrLn "2:"
     >> display (moves' b)
     >> putStrLn "3:"
     >> display (moves'' b)

blankBoard :: Int -> Int -> Board
blankBoard x y = Board $ replicate (fromIntegral x) (replicate (fromIntegral y) Nothing)

update'' :: Position -> Direction -> [Tiles.PlayedTile] -> Board -> Maybe Board
update'' p Horizontal ts b = update' p Horizontal ts b
update'' p Vertical ts b = transpose <$> update' (swap p) Horizontal ts (transpose b)

update' :: Position -> Direction -> [Tiles.PlayedTile] -> Board -> Maybe Board
update' _ _ _ (Board []) = Nothing
update' _ _ [] b = Just b
update' p _ [t] b = updateBoard p t b
update' p d (t:ts) b = do
  b' <- updateBoard p t b
  update' (forward d p) d ts b'

updateBoard :: Position -> Tiles.PlayedTile -> Board -> Maybe Board
updateBoard _ _ (Board []) = Nothing
updateBoard (Position x 0) t (Board (r:rs)) = fmap ((: rs) >>> Board) (updateRow r x t)
updateBoard (Position x y) t (Board (r:rs)) =
  fmap (\(Board rs) -> Board (r : rs))
  (updateBoard (Position x (y - 1)) t (Board rs))

updateRow :: Row -> Int -> Tiles.PlayedTile -> Maybe Row
updateRow [] _ _ = Nothing
updateRow (c@(Just _) : cs) 0 t = (:) c <$> updateRow cs 0 t
updateRow (c : cs) 0 t = fmap ((: cs) . Just) (updateCell c t)
updateRow (c : cs) p t = fmap ((:) c) (updateRow cs (p - 1) t)

updateCell :: Maybe Tiles.PlayedTile -> Tiles.PlayedTile -> Maybe Tiles.PlayedTile
updateCell Nothing t = Just t
updateCell _ _ = Nothing

transpose :: Board -> Board
transpose (Board rs) = Board $ foldr (zipWith (:)) (replicate (length rs) []) rs

swap :: Position -> Position
swap (Position x y) = Position y x

data Direction = Horizontal | Vertical deriving (Show)
instance Enum Direction where
  fromEnum Horizontal = 0
  fromEnum Vertical = 1
  toEnum = bool Horizontal Vertical . odd

forward :: Direction -> Position -> Position
forward Horizontal p = p { positionX = positionX p + 1 }
forward Vertical p = p { positionY = positionY p + 1 }

backward :: Direction -> Position -> Position
backward Horizontal p = p { positionX = positionX p - 1 }
backward Vertical p = p { positionY = positionY p - 1 }

data Position = Position
  { positionX :: Int
  , positionY :: Int
  } deriving (Show, Eq)

showBoard :: Board -> String
showBoard (Board rs) = let
  inner = "\n" <> replicate 20 '-' <> "\n"
  in fold . intersperse "\n" . fmap showRow $ rs

showRow :: Row -> String
showRow = fmap showCell >>> intersperse "|" >>> fold

showCell :: Maybe Tiles.PlayedTile -> String
showCell Nothing = "_"
showCell (Just (Tiles.PlayedBlank c)) = [c]
showCell _ = undefined

data SquareType = NormalSquare | WordMultiplier Int | LetterMultiplier Int
data Square = Square SquareType (Maybe Tiles.PlayedTile)

boardFrom :: Board.Position -> Board -> Maybe Board
boardFrom _ (Board []) = Nothing
boardFrom (Board.Position 0 0) b = Just b
boardFrom (Board.Position x 0) (Board rs) = Board <$> traverse (rowFrom x) rs
boardFrom (Board.Position x y) (Board (_ : rs)) = boardFrom (Board.Position x (y - 1)) (Board rs)

rowFrom :: Int -> Board.Row -> Maybe Board.Row
rowFrom _ [] = Nothing
rowFrom 0 cs = Just cs
rowFrom p (_ : cs) = rowFrom (p - 1) cs

columnAt :: Int -> Board -> Maybe Board.Column
columnAt i (Board rs) = traverse listToMaybe $ drop (fromIntegral i) rs

modifyColumn :: (Board.Column -> Board.Column) -> Int -> Board -> Maybe Board
modifyColumn f i b = do
  let i' = fromIntegral i
  c <- columnAt i b
  setColumn i' (f c) b

setColumn :: Int -> Board.Column -> Board -> Maybe Board
setColumn i c (Board rs) = let
  setCell :: Int -> Maybe Tiles.PlayedTile -> Board.Row -> Board.Row
  setCell j cell r = take j r <> [cell] <> drop (j + 1) r
  in if i < length rs
     then Just $ Board $  zipWith (setCell i) c rs
     else Nothing
