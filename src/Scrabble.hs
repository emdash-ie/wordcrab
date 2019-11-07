{-# LANGUAGE DeriveGeneric #-}

module Scrabble where

import           Control.Applicative (liftA2)
import           Control.Monad.Trans.State.Lazy
import           Data.Aeson (ToJSON, FromJSON)
import           Data.Bool (bool)
import           Data.Foldable (traverse_)
import           Data.Functor ((<&>))
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import           Data.Maybe (isJust, listToMaybe)
import           GHC.Generics
import           System.Random (randomRs, getStdGen)

data GameState = GameState
  { tileBag :: [Tile]
  , players :: [Player] -- maybe a Set?
  , turnOrder :: [Player]
  , board :: Board
  }

type Board = [Row]
type Row = [Maybe PlayedTile]
data SquareType = NormalSquare | WordMultiplier Integer | LetterMultiplier Integer
data BoardSquare = BoardSquare SquareType (Maybe PlayedTile)
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

data Player = Player
  { name :: String
  , rack :: [Tile]
  , currentScore :: Integer
  } deriving (Generic, Show)

instance ToJSON Player where
  -- toEncoding = genericToEncoding defaultOptions

instance FromJSON Player

data Position = Position
  { positionX :: Integer
  , positionY :: Integer
  } deriving Show

type Score = Integer
type WordResult = NonEmpty (PlayedTile, SquareType)
data PlayResult = PlayResult
  { mainWord :: WordResult
  , sideWords :: [WordResult]
  , rest :: Maybe Play
  }

instance Semigroup PlayResult where
  pr <> pr' = PlayResult
    { mainWord = mainWord pr <> mainWord pr'
    , sideWords = sideWords pr <> sideWords pr'
    , rest = let f = (>>= restOfPlay) . rest
             in  f pr <> f pr'
    }

data Direction = Horizontal | Vertical deriving (Show)
instance Enum Direction where
  fromEnum Horizontal = 0
  fromEnum Vertical = 1
  toEnum = bool Horizontal Vertical . odd

data Play = Play
  { tiles :: NonEmpty PlayedTile
  , direction :: Direction
  , startPosition :: Position
  } deriving (Show)

instance Semigroup Play where
  p <> p' = p { tiles = tiles p <> tiles p' }

play :: GameState -> Play -> Maybe (Score, GameState)
play gs p = validatePlay gs p <&> undefined

restOfPlay :: Play -> Maybe Play
restOfPlay (Play ts d p) = NE.nonEmpty (NE.tail ts)
                           <&> \ts' -> Play ts' d (forward d p)

forward :: Direction -> Position -> Position
forward Horizontal p = p { positionX = positionX p + 1 }
forward Vertical p = p { positionY = positionY p + 1 }

playTiles :: GameState -> Play -> Maybe (PlayResult, GameState)
playTiles gs p = let
  -- if the current square is empty:
  --   place a tile and get the perpendicular word (concat both directions, one reversed)
  --   include the square's multiplier
  (result, gs') = playTile gs p
  -- always:
  --   add the tile on the current square to the main word
  -- finish:
  --   out of tiles in the play (success) or out of spaces on the board (error)
  in case restOfPlay p of
    Nothing -> undefined
    Just p' -> undefined

playTile :: GameState -> Play -> (PlayResult, GameState)
playTile = let
  in undefined

updateBoard :: Position -> PlayedTile -> Board -> Maybe Board
updateBoard _ _ [] = Nothing
updateBoard (Position x 0) t (r:rs) = fmap (: rs) (updateRow r x t)
updateBoard (Position x y) t (r:rs) = fmap ((:) r) (updateBoard (Position x (y - 1)) t rs)

updateRow :: Row -> Integer -> PlayedTile -> Maybe Row
updateRow [] _ _ = Nothing
updateRow (c : cs) 0 t = fmap ((: cs) . Just) (updateCell c t)
updateRow (c : cs) p t = fmap ((:) c) (updateRow cs (p - 1) t)

updateCell :: Maybe PlayedTile -> PlayedTile -> Maybe PlayedTile
updateCell Nothing t = Just t
updateCell _ _ = Nothing

perpWord :: GameState -> Position -> Direction -> PlayedTile -> [BoardSquare]
perpWord = undefined

perpendicular :: Direction -> Direction
perpendicular = succ

placeTile :: GameState -> Position -> GameState
placeTile = undefined

positionIsEmpty :: GameState -> Position -> Bool
positionIsEmpty gs p = isJust $ fst $ squareAt gs p

squareAt :: GameState -> Position -> (Maybe Char, BoardSquare)
squareAt = undefined

wordAt :: GameState -> Position -> Direction -> [(Char, BoardSquare)]
wordAt = undefined

scorePlay :: PlayResult -> Score
scorePlay = undefined

validatePlay :: GameState -> Play -> Maybe Play
validatePlay = undefined

-- play requirements:
-- all played tiles in one line (all same x or same y)
-- at least one played tile neighbouring at least one existing tile
-- continuous line made by played tiles plus existing tiles

nextTurn :: GameState -> GameState
nextTurn gs = gs {turnOrder = (tail . turnOrder) gs}

main :: IO ()
main = do
  gen <- getStdGen
  let rs = randomRs (0, 39) gen :: [Int]
  traverse_ print (take 10 rs)
