{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}

module Wordcrab.Tiles where

import Control.Monad (join)
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics
import System.Random (StdGen)
import System.Random.Shuffle (shuffle')

data Tile
  = Blank
  | Letter LetterTile
  deriving (Generic, Show, Eq)
instance ToJSON Tile
instance FromJSON Tile

data PlayedTile
  = PlayedBlank Char
  | PlayedLetter LetterTile
  deriving (Show, Generic)
instance ToJSON PlayedTile
instance FromJSON PlayedTile

data LetterTile = LetterTile
  { letter :: Char
  , score :: Integer
  }
  deriving (Generic, Show, Eq)
instance ToJSON LetterTile
instance FromJSON LetterTile

tileScore :: PlayedTile -> Integer
tileScore (PlayedBlank _) = 0
tileScore (PlayedLetter lt) = score lt

shuffleBag :: StdGen -> [Tile] -> [Tile]
shuffleBag gen ts = shuffle' ts (length ts) gen

blanks :: String -> [PlayedTile]
blanks = fmap PlayedBlank

showTile :: PlayedTile -> String
showTile = \case
  PlayedBlank c -> [c]
  PlayedLetter lt -> [letter lt]

unplay :: PlayedTile -> Tile
unplay (PlayedBlank _) = Blank
unplay (PlayedLetter lt) = Letter lt

tileset :: [Tile]
tileset =
  join
    [ replicate 2 Blank
    , replicate 9 $ Letter a
    , replicate 2 $ Letter b
    , replicate 2 $ Letter c
    , replicate 4 $ Letter d
    , replicate 12 $ Letter e
    , replicate 2 $ Letter f
    , replicate 3 $ Letter g
    , replicate 2 $ Letter h
    , replicate 9 $ Letter i
    , replicate 1 $ Letter j
    , replicate 1 $ Letter k
    , replicate 4 $ Letter l
    , replicate 2 $ Letter m
    , replicate 6 $ Letter n
    , replicate 8 $ Letter o
    , replicate 2 $ Letter p
    , replicate 1 $ Letter q
    , replicate 6 $ Letter r
    , replicate 4 $ Letter s
    , replicate 6 $ Letter t
    , replicate 4 $ Letter u
    , replicate 2 $ Letter v
    , replicate 2 $ Letter w
    , replicate 1 $ Letter x
    , replicate 2 $ Letter y
    , replicate 1 $ Letter z
    ]

a :: LetterTile
a = LetterTile 'A' 1

b :: LetterTile
b = LetterTile 'B' 3

c :: LetterTile
c = LetterTile 'C' 3

d :: LetterTile
d = LetterTile 'D' 2

e :: LetterTile
e = LetterTile 'E' 1

f :: LetterTile
f = LetterTile 'F' 4

g :: LetterTile
g = LetterTile 'G' 2

h :: LetterTile
h = LetterTile 'H' 4

i :: LetterTile
i = LetterTile 'I' 1

j :: LetterTile
j = LetterTile 'J' 8

k :: LetterTile
k = LetterTile 'K' 5

l :: LetterTile
l = LetterTile 'L' 1

m :: LetterTile
m = LetterTile 'M' 3

n :: LetterTile
n = LetterTile 'N' 1

o :: LetterTile
o = LetterTile 'O' 1

p :: LetterTile
p = LetterTile 'P' 3

q :: LetterTile
q = LetterTile 'Q' 10

r :: LetterTile
r = LetterTile 'R' 1

s :: LetterTile
s = LetterTile 'S' 1

t :: LetterTile
t = LetterTile 'T' 1

u :: LetterTile
u = LetterTile 'U' 1

v :: LetterTile
v = LetterTile 'V' 4

w :: LetterTile
w = LetterTile 'W' 4

x :: LetterTile
x = LetterTile 'X' 8

y :: LetterTile
y = LetterTile 'Y' 4

z :: LetterTile
z = LetterTile 'Z' 10
