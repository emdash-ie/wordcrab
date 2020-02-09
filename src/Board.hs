module Board
  ( Position(..)
  , Direction(..)
  , forward
  , backward
  )
where

import Data.Bool (bool)

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
