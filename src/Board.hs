module Board where

import Data.Bool (bool)
import Control.Category ((>>>))
import Control.Monad ((>=>))
import Data.Foldable (fold)
import Data.List (intersperse)

import qualified Tiles

type Board = [Row]
type Row = [Maybe Tiles.PlayedTile]
type Column = [Maybe Tiles.PlayedTile]

testBoard :: IO (Maybe ())
testBoard = let
  display = traverse (putStrLn . showBoard)
  b = blankBoard 10 10
  moves = updateBoard' (Position 2 5) Horizontal (Tiles.blanks "ab")
          >=> updateBoard' (Position 1 5) Horizontal (Tiles.blanks "tle")
          >=> updateBoard' (Position 4 2) Vertical (Tiles.blanks "helo")
  moves' = updateBoard'' (Position 2 5) Horizontal (Tiles.blanks "ab")
           >=> updateBoard'' (Position 1 5) Horizontal (Tiles.blanks "tle")
           >=> updateBoard'' (Position 4 2) Vertical (Tiles.blanks "helo")
  moves'' = moves' >=> updateBoard'' (Position 1 6) Vertical (Tiles.blanks "hinking")
  in putStrLn "1:"
     >> display (moves b)
     >> putStrLn "2:"
     >> display (moves' b)
     >> putStrLn "3:"
     >> display (moves'' b)

blankBoard :: Integer -> Integer -> Board
blankBoard x y = replicate (fromIntegral x) (replicate (fromIntegral y) Nothing)

updateBoard'' :: Position -> Direction -> [Tiles.PlayedTile] -> Board -> Maybe Board
updateBoard'' p Horizontal ts b = updateBoard' p Horizontal ts b
updateBoard'' p Vertical ts b = transpose <$> updateBoard' (swap p) Horizontal ts (transpose b)


updateBoard' :: Position -> Direction -> [Tiles.PlayedTile] -> Board -> Maybe Board
updateBoard' _ _ _ [] = Nothing
updateBoard' _ _ [] b = Just b
updateBoard' p _ [t] b = updateBoard p t b
updateBoard' p d (t:ts) b = do
  b' <- updateBoard p t b
  updateBoard' (forward d p) d ts b'

-- Maybe add a Direction parameter to this to decide whether to updateRow or
-- updateColumn?
updateBoard :: Position -> Tiles.PlayedTile -> Board -> Maybe Board
updateBoard _ _ [] = Nothing
updateBoard (Position x 0) t (r:rs) = fmap (: rs) (updateRow r x t)
updateBoard (Position x y) t (r:rs) = fmap ((:) r) (updateBoard (Position x (y - 1)) t rs)

updateRow :: Row -> Integer -> Tiles.PlayedTile -> Maybe Row
updateRow [] _ _ = Nothing
updateRow (c@(Just _) : cs) 0 t = (:) c <$> updateRow cs 0 t
updateRow (c : cs) 0 t = fmap ((: cs) . Just) (updateCell c t)
updateRow (c : cs) p t = fmap ((:) c) (updateRow cs (p - 1) t)

updateCell :: Maybe Tiles.PlayedTile -> Tiles.PlayedTile -> Maybe Tiles.PlayedTile
updateCell Nothing t = Just t
updateCell _ _ = Nothing

transpose :: Board -> Board
transpose b = foldr (zipWith (:)) (replicate (length b) []) b

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

data Position = Position
  { positionX :: Integer
  , positionY :: Integer
  } deriving Show

showBoard :: Board -> String
showBoard = let
  inner = "\n" <> replicate 20 '-' <> "\n"
  in fold . intersperse "\n" . fmap showRow

showRow :: Row -> String
showRow = fmap showCell >>> intersperse "|" >>> fold

showCell :: Maybe Tiles.PlayedTile -> String
showCell Nothing = "_"
showCell (Just (Tiles.PlayedBlank c)) = [c]
showCell _ = undefined

data SquareType = NormalSquare | WordMultiplier Integer | LetterMultiplier Integer
data Square = Square SquareType (Maybe Tiles.PlayedTile)
