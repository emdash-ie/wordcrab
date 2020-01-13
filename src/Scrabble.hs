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

playTile :: GameState -> Play -> (PlayResult, GameState)
playTile = let
  in undefined

boardFrom :: Board.Position -> Board -> Maybe Board
boardFrom _ [] = Nothing
boardFrom (Board.Position 0 0) b = Just b
boardFrom (Board.Position x 0) rs = traverse (rowFrom x) rs
boardFrom (Board.Position x y) (_ : rs) = boardFrom (Board.Position x (y - 1)) rs

rowFrom :: Integer -> Board.Row -> Maybe Board.Row
rowFrom _ [] = Nothing
rowFrom 0 cs = Just cs
rowFrom p (c : cs) = rowFrom (p - 1) cs

columnAt :: Integer -> Board -> Maybe Board.Column
columnAt = fromIntegral >>> drop >>> (>>> listToMaybe) >>> traverse

modifyColumn :: (Board.Column -> Board.Column) -> Integer -> Board -> Maybe Board
modifyColumn f i b = do
  let i' = fromIntegral i
  c <- columnAt i b
  setColumn i' (f c) b

setColumn :: Int -> Board.Column -> Board -> Maybe Board
setColumn i c b = let
  setCell :: Int -> Maybe Tiles.PlayedTile -> Board.Row -> Board.Row
  setCell j cell r = take j r <> [cell] <> drop (j + 1) r
  in if i < length b
     then Just $ zipWith (setCell i) c b
     else Nothing

perpWord :: GameState -> Board.Position -> Board.Direction -> Tiles.PlayedTile -> [Board.Square]
perpWord = undefined

perpendicular :: Board.Direction -> Board.Direction
perpendicular = succ

placeTile :: GameState -> Board.Position -> GameState
placeTile = undefined

positionIsEmpty :: GameState -> Board.Position -> Bool
positionIsEmpty gs p = isJust $ fst $ squareAt gs p

squareAt :: GameState -> Board.Position -> (Maybe Char, Board.Square)
squareAt = undefined

wordAt :: GameState -> Board.Position -> Board.Direction -> [(Char, Board.Square)]
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
