{-# LANGUAGE DeriveGeneric #-}

module Wordcrab.Board (
  Board (..),
  Direction (..),
  PlayedWhen (..),
  Position (..),
  ValidPosition,
  TileInPlay,
  Square (..),
  SquareType (..),
  Play,
  Row (..),
  PlayError (..),
  forward,
  backward,
  play,
  showBoard,
  blankBoard,
  totalWordMultiplier,
  tileMultiplier,
  horizontalNeighbours,
  validatePosition,
) where

import Prelude hiding (lookup)

import Control.Category ((>>>))
import Data.Aeson (FromJSON, ToJSON)
import Data.Bifunctor (first, second)
import Data.Bool (bool)
import Data.Foldable (fold)
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.List (intersperse)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Maybe (catMaybes, isJust)
import Data.Vector (Vector)
import qualified Data.Vector as V
import GHC.Generics (Generic)

newtype Board t = Board {unBoard :: Vector (Row t)} deriving (Generic)
instance ToJSON t => ToJSON (Board t)
instance FromJSON t => FromJSON (Board t)

newtype Row t = Row {unRow :: Vector (Square (Maybe t))} deriving (Generic)
instance ToJSON t => ToJSON (Row t)
instance FromJSON t => FromJSON (Row t)

data SquareType = Normal | WordMultiplier Int | LetterMultiplier Int
  deriving (Show, Generic)
instance ToJSON SquareType
instance FromJSON SquareType

data Square a = Square {squareType :: SquareType, squareContents :: a}
  deriving (Show, Generic)
instance ToJSON a => ToJSON (Square a)
instance FromJSON a => FromJSON (Square a)

isWordMultiplier :: SquareType -> Bool
isWordMultiplier (WordMultiplier _) = True
isWordMultiplier _ = False

totalWordMultiplier :: [SquareType] -> Int
totalWordMultiplier =
  filter isWordMultiplier
    >>> fmap (\(WordMultiplier n) -> n)
    >>> product

tileMultiplier :: PlayedWhen -> SquareType -> Int
tileMultiplier PlayedEarlier _ = 1
tileMultiplier _ t = case t of
  LetterMultiplier i -> i
  _ -> 1

newtype ValidPosition = ValidPosition {unwrapPosition :: Position}
  deriving (Eq, Show, Generic)

instance ToJSON ValidPosition
instance FromJSON ValidPosition

validatePosition :: Position -> Board t -> Maybe ValidPosition
validatePosition p (Board rs) =
  let validY = positionY p < fromIntegral (length rs) && positionY p > 0
      validX = case rs V.!? 0 of
        Nothing -> False
        Just (Row ts) -> positionX p < fromIntegral (length ts) && positionX p > 0
   in bool Nothing (Just $ ValidPosition p) (validY && validX)

blankBoard :: Board t
blankBoard =
  let squares = (fmap . fmap) (\t -> Square t Nothing) squareTypes
   in Board $ V.fromList $ fmap (V.fromList >>> Row) squares

squareTypes :: [[SquareType]]
squareTypes =
  let n = Normal
      dl = LetterMultiplier 2
      tl = LetterMultiplier 3
      dw = WordMultiplier 2
      tw = WordMultiplier 3
      upperLeft =
        [ [tw, n, n, dl, n, n, n, tw]
        , [n, dw, n, n, n, tl, n, n]
        , [n, n, dw, n, n, n, dl, n]
        , [dl, n, n, dw, n, n, n, dl]
        , [n, n, n, n, dw, n, n, n]
        , [n, tl, n, n, n, tl, n, n]
        , [n, n, dl, n, n, n, dl, n]
        , [tw, n, n, dl, n, n, n, dw]
        ]
      reflect xs = xs <> tail (reverse xs)
   in reflect (fmap reflect upperLeft)

lookup :: Board t -> ValidPosition -> Square (Maybe t)
lookup (Board rs) (ValidPosition p) = unRow (rs V.! positionY p) V.! positionX p

update :: Board t -> ValidPosition -> t -> Maybe (Board t)
update b p t = case squareContents $ lookup b p of
  Just _ -> Nothing
  Nothing -> Just $ write b p t

write :: Board t -> ValidPosition -> t -> Board t
write (Board rs) (ValidPosition p) t =
  let Row oldRow = rs V.! positionY p
      s = oldRow V.! positionX p
      newRow = Row $ oldRow V.// [(positionX p, Square (squareType s) (Just t))]
   in Board (rs V.// [(positionY p, newRow)])

writeSeveral :: Foldable a => a (ValidPosition, t) -> Board t -> Board t
writeSeveral xs b = foldr (\(p, t) b' -> write b' p t) b xs

playIndices ::
  Position ->
  Direction ->
  NonEmpty t ->
  Board t ->
  Maybe (NonEmpty (ValidPosition, Square t))
playIndices p d ts b = do
  vp <- validatePosition p b
  case lookup b vp of
    Square _ (Just _) -> playIndices (forward d p) d ts b
    Square st Nothing -> case NE.uncons ts of
      (t, Nothing) -> Just $ (vp, Square st t) :| []
      (t, Just ts') ->
        NE.cons (vp, Square st t)
          <$> playIndices (forward d p) d ts' b

bordersWord ::
  NonEmpty ValidPosition ->
  Board t ->
  Bool
bordersWord ps b =
  let hasNeighbours p = any (squareContents >>> isJust) (neighbours p b)
   in any hasNeighbours ps

neighbours ::
  ValidPosition ->
  Board t ->
  NonEmpty (Square (Maybe t))
neighbours p b = horizontalNeighbours p b <> verticalNeighbours p b

horizontalNeighbours ::
  ValidPosition ->
  Board t ->
  NonEmpty (Square (Maybe t))
horizontalNeighbours (ValidPosition p) b =
  let f p' = validatePosition p' b <&> lookup b
      left = f (backward Horizontal p)
      right = f (forward Horizontal p)
   in NE.fromList (catMaybes [left, right])

verticalNeighbours ::
  ValidPosition ->
  Board t ->
  NonEmpty (Square (Maybe t))
verticalNeighbours (ValidPosition p) b =
  let f p' = validatePosition p' b <&> lookup b
      above = f (backward Vertical p)
      below = f (forward Vertical p)
   in NE.fromList (catMaybes [above, below])

wordAt ::
  ValidPosition ->
  Direction ->
  Board t ->
  [(ValidPosition, Square t)]
wordAt p d b = wordFrom (startOfWord p d b) d b

startOfWord ::
  ValidPosition ->
  Direction ->
  Board t ->
  ValidPosition
startOfWord vp@(ValidPosition p) d b =
  case validatePosition (backward d p) b of
    Nothing -> vp
    Just p' -> case lookup b p' of
      Square _ Nothing -> vp
      Square _ (Just _) -> startOfWord p' d b

wordFrom ::
  ValidPosition ->
  Direction ->
  Board t ->
  [(ValidPosition, Square t)]
wordFrom vp@(ValidPosition p) d b = case lookup b vp of
  Square _ Nothing -> []
  Square st (Just t) -> case validatePosition (forward d p) b of
    Nothing -> [(vp, Square st t)]
    Just vp' -> (vp, Square st t) : wordFrom vp' d b

type Play t = (Board t, NonEmpty (TileInPlay t), [[TileInPlay t]])
type TileInPlay t = (PlayedWhen, Square t)
data PlayedWhen = PlayedNow | PlayedEarlier deriving (Show, Eq, Generic)
instance ToJSON PlayedWhen
instance FromJSON PlayedWhen

play ::
  Position ->
  Direction ->
  NonEmpty t ->
  (t -> Integer) ->
  Board t ->
  Either (PlayError t) (Play t, Integer)
play p d ts tileScore b = do
  indices <- maybe (Left NonExistentPositions) Right $ playIndices p d ts b
  let playedIndices = fmap fst indices
  if bordersWord playedIndices b
    || elem (Position 7 7) (fmap (fst >>> unwrapPosition) indices)
    then Right ()
    else Left $ InvalidPositions indices
  let b' = writeSeveral (NE.map (second (\(Square _ x) -> x)) indices) b
  let mainWord = NE.fromList $ wordAt (NE.head playedIndices) d b'
  let perpWords = NE.filter ((> 1) . length) $ fmap (\i -> wordAt i (succ d) b') playedIndices
  let active = first (\i -> bool PlayedEarlier PlayedNow (i `elem` playedIndices))
  let play = (b', fmap active mainWord, (fmap . fmap) active perpWords)
  pure (play, score tileScore play)

data PlayError t
  = NonExistentPositions
  | InvalidPositions (NonEmpty (ValidPosition, Square t))
  deriving (Show, Generic)

instance ToJSON t => ToJSON (PlayError t)
instance FromJSON t => FromJSON (PlayError t)

score :: (t -> Integer) -> Play t -> Integer
score tileScore (_, mainWord, perpWords) =
  scoreWord tileScore (NE.toList mainWord) + sum (fmap (scoreWord tileScore) perpWords)

scoreWord :: (t -> Integer) -> [TileInPlay t] -> Integer
scoreWord tileScore ts =
  let activeMultipliers =
        snd . second squareType
          <$> filter (fst >>> (==) PlayedNow) ts
      wordMultiplier = fromIntegral $ totalWordMultiplier activeMultipliers
      letterScore (PlayedNow, Square (LetterMultiplier n) t) = fromIntegral n * tileScore t
      letterScore (_, Square _ t) = tileScore t
   in wordMultiplier * sum (fmap letterScore ts)

showBoard :: (t -> String) -> Board t -> String
showBoard showT (Board rs) =
  let inner = "\n" <> replicate 20 '-' <> "\n"
   in fold . intersperse "\n" . fmap (showRow showT) $ V.toList rs

showRow :: (t -> String) -> Row t -> String
showRow showT (Row ts) = fmap (showCell showT) ts & V.toList & intersperse "|" & fold

showCell :: (t -> String) -> Square (Maybe t) -> String
showCell _ (Square _ Nothing) = "_"
showCell showT (Square _ (Just t)) = showT t

data Direction = Horizontal | Vertical deriving (Show, Generic)
instance Enum Direction where
  fromEnum Horizontal = 0
  fromEnum Vertical = 1
  toEnum = bool Horizontal Vertical . odd
instance ToJSON Direction
instance FromJSON Direction

forward :: Direction -> Position -> Position
forward Horizontal p = p{positionX = positionX p + 1}
forward Vertical p = p{positionY = positionY p + 1}

backward :: Direction -> Position -> Position
backward Horizontal p = p{positionX = positionX p - 1}
backward Vertical p = p{positionY = positionY p - 1}

data Position = Position
  { positionX :: Int
  , positionY :: Int
  }
  deriving (Show, Eq, Generic)
instance ToJSON Position
instance FromJSON Position
