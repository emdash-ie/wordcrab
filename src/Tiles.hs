{-# LANGUAGE DeriveGeneric #-}

module Tiles where

import Data.Aeson (ToJSON, FromJSON)
import GHC.Generics

data Tile = Blank | Letter LetterTile deriving (Generic, Show)
instance ToJSON Tile
instance FromJSON Tile
data PlayedTile = PlayedBlank Char | PlayedLetter LetterTile deriving (Show)
data LetterTile = LetterTile
  { letter :: Char
  , score :: Integer
  } deriving (Generic, Show)
instance ToJSON LetterTile
instance FromJSON LetterTile

blanks :: String -> [Tiles.PlayedTile]
blanks = fmap Tiles.PlayedBlank
