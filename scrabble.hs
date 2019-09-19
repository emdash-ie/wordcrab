{-# LANGUAGE DeriveGeneric #-}

module Scrabble where

import System.Random (randomRs, getStdGen)
import Data.Foldable (traverse_)
import GHC.Generics
import Data.Aeson (ToJSON, FromJSON)

data GameState = GameState
  { tileBag :: [Tile]
  , players :: [Player]
  , turnOrder :: [Player]
  , board :: [[Maybe PlayedTile]]
  }

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
  { x :: Int
  , y :: Int
  }

data PlayError = UnconnectedTiles

playWord :: GameState -> Play -> Either PlayError GameState
playWord gs p = undefined

nextTurn :: GameState -> GameState
nextTurn gs = gs {turnOrder = (drop 1 . turnOrder) gs}

main :: IO ()
main = do
  gen <- getStdGen
  let rs = randomRs (0, 39) gen :: [Int]
  traverse_ print (take 10 rs)
