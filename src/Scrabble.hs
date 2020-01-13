{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TupleSections #-}

module Scrabble where

import           Control.Applicative (liftA2)
import           Control.Category ((>>>))
import           Control.Monad ((>=>))
import           Control.Monad.Trans.State.Lazy
import           Data.Aeson (ToJSON, FromJSON)
import           Data.Bifunctor (bimap)
import           Data.Bool (bool)
import           Data.Foldable (traverse_, fold)
import           Data.Functor ((<&>))
import           Data.List (intersperse)
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import           Data.Maybe (isJust, listToMaybe)
import           Debug.Trace (trace)
import           GHC.Generics
import           System.Random (randomRs, getStdGen)
import qualified Tiles
import           Tiles (Tile)
import qualified Board
import           Board (Board)

data GameState = GameState
  { tileBag :: [Tile]
  , players :: [Player] -- maybe a Set?
  , turnOrder :: [Player]
  , board :: Board
  }

data Player = Player
  { name :: String
  , rack :: [Tile]
  , currentScore :: Integer
  } deriving (Generic, Show)

instance ToJSON Player where
  -- toEncoding = genericToEncoding defaultOptions

instance FromJSON Player

type Score = Integer
type WordResult = NonEmpty (Tiles.PlayedTile, Board.SquareType)
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

data Play = Play
  { tiles :: NonEmpty Tiles.PlayedTile
  , direction :: Board.Direction
  , startPosition :: Board.Position
  } deriving (Show)

instance Semigroup Play where
  p <> p' = p { tiles = tiles p <> tiles p' }

play :: GameState -> Play -> Maybe (Score, GameState)
play gs p = validatePlay gs p <&> undefined

restOfPlay :: Play -> Maybe Play
restOfPlay (Play ts d p) = NE.nonEmpty (NE.tail ts)
                           <&> \ts' -> Play ts' d (Board.forward d p)

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
