{-# LANGUAGE DeriveGeneric #-}

module Scrabble where

import Data.Aeson (ToJSON, FromJSON)
import Data.Foldable (traverse_)
import qualified Data.List.NonEmpty as NE
import Data.List.NonEmpty (NonEmpty(..))
import GHC.Generics
import System.Random (randomRs, getStdGen)

data GameState = GameState
  { tileBag :: [Tile]
  , players :: [Player] -- maybe a Set?
  , turnOrder :: [Player]
  , board :: Board
  }

type Board = [[Maybe PlayedTile]]
data Tile = Blank | Letter LetterTile deriving (Generic, Show)
instance ToJSON Tile
instance FromJSON Tile
data PlayedTile = PlayedBlank Char | PlayedLetter LetterTile
newtype Play = Play [(Position, PlayedTile)]
data LetterTile = LetterTile
  { letter :: Char
  , score :: Int
  } deriving (Generic, Show)
instance ToJSON LetterTile
instance FromJSON LetterTile

data Player = Player
  { name :: String
  , tiles :: [Tile]
  , currentScore :: Int
  } deriving (Generic, Show)

instance ToJSON Player where
  -- toEncoding = genericToEncoding defaultOptions

instance FromJSON Player

data Position = Position
  { positionX :: Int
  , positionY :: Int
  }

data PlayError = UnconnectedTiles | EmptyPlay | OutOfLine

playWord :: GameState -> Play -> Either PlayError GameState
playWord gs p = do
  if validPlay (board gs) p
    then undefined
    else undefined
  undefined

-- maybe makes more sense as playWords :: Board -> Play -> Either PlayError [PlayWord]
validPlay :: Board -> Play -> Bool
validPlay b p = let
  -- all played tiles in one line (all same x or same y)
  -- at least one played tile neighbouring at least one existing tile
  -- continuous line made by played tiles plus existing tiles
  in undefined

playWords :: Board -> Play -> Either PlayError [PlayWord]
playWords b (Play (t:ts)) = let
  x = positionX $ fst t
  y = positionY $ fst t
  in if all ((== x) . positionX . fst) ts
  then playVertical b (t :| ts)
  else if all ((== y) . positionY . fst) ts
       then playHorizontal b (t :| ts)
       else Left OutOfLine
playWords _ _ = Left EmptyPlay

playHorizontal :: Board -> NonEmpty (Position, PlayedTile) -> Either PlayError [PlayWord]
playHorizontal b ((p, tile) :| ts) = undefined

playVertical :: Board -> NonEmpty (Position, PlayedTile) -> Either PlayError [PlayWord]
playVertical b ((p, tile) :| ts) = undefined

data PlayWord = PlayWord

nextTurn :: GameState -> GameState
nextTurn gs = gs {turnOrder = (tail . turnOrder) gs}

main :: IO ()
main = do
  gen <- getStdGen
  let rs = randomRs (0, 39) gen :: [Int]
  traverse_ print (take 10 rs)
