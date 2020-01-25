{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
import           Prelude hiding (Word)
import           System.Random (randomRs, getStdGen)
import qualified Tiles
import           Tiles (Tile)
import qualified Board
import           Board (Board(..))

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

restOfPlay :: Play -> Maybe Play
restOfPlay (Play ts d p) = NE.nonEmpty (NE.tail ts)
                           <&> \ts' -> Play ts' d (Board.forward d p)

playTiles :: GameState -> Play -> Maybe (PlayResult, GameState)
playTiles gs p = let
  -- if the current square is empty:
  --   place a tile and get the perpendicular word (concat both directions, one reversed)
  --   include the square's multiplier
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

-- take a play, determine whether it’s valid (fits on the board)
  -- if so, return the squares the play is on
  -- if not, return an error
validate :: Play -> Board -> ValidatedPlay
validate p b = case direction p of
  Board.Horizontal -> update' p b
  Board.Vertical -> let
    vp = update' (swap p) (Board.transpose b)
    transpose' = \pr@PlayResult{..} -> pr { resultingBoard = Board.transpose resultingBoard }
    in fmap transpose' vp

swap :: Play -> Play
swap p@Play{..} = p { startPosition = Board.swap startPosition }

update' :: Play -> Board -> ValidatedPlay
update' _ (Board []) = Left TooLong
update' p b = let
  partialResult = update (startPosition p) (direction p) (NE.head $ tiles p) b
  in case restOfPlay p of
     Nothing -> partialResult
     Just p' -> do
       pr <- partialResult
       update' p' (resultingBoard pr)

update ::
  Board.Position ->
  Board.Direction ->
  Tiles.PlayedTile ->
  Board ->
  ValidatedPlay
update _ _ _ (Board []) = Left TooLong
update (Board.Position x 0) d t (Board (r:rs)) = do
  (updatedRow, mainWord, perpWord) <- updateRow r x t
  let updatedBoard = Board (updatedRow : rs)
  pure (PlayResult [mainWord] [perpWord] updatedBoard)
update (Board.Position x y) d t (Board (r:rs)) =
  fmap (\(p@PlayResult{..}) -> p { resultingBoard = Board (r : rs) })
  (update (Board.Position x (y - 1)) d t (Board rs))

updateRow ::
  Board.Row ->
  Integer ->
  Tiles.PlayedTile ->
  Either PlayError (Board.Row, (Board.SquareType, Tile), Word)
updateRow [] _ _ = Left TooLong
updateRow (c@(Just _) : cs) 0 t = do
  (row, played, perpWord) <- updateRow cs 0 t
  pure (c : row, played, perpWord)
updateRow (c : cs) 0 t = Right $ updateCell c t : cs
updateRow (c : cs) p t = fmap ((:) c) (updateRow cs (p - 1) t)

updateCell :: Maybe Tiles.PlayedTile -> Tiles.PlayedTile -> Maybe Tiles.PlayedTile
updateCell Nothing t = Just t
updateCell _ _ = Nothing

data PlayError = TooLong

type ValidatedPlay = Either PlayError PlayResult

data PlayResult = PlayResult
  { mainWord :: Word
  , sideWords :: [Word]
  , resultingBoard :: Board
  }

type Score = Integer

type Word = [(Board.SquareType, Tile)]

data Play = Play
  { tiles :: NonEmpty Tiles.PlayedTile
  , direction :: Board.Direction
  , startPosition :: Board.Position
  } deriving (Show)

instance Semigroup Play where
  p <> p' = p { tiles = tiles p <> tiles p' }

main :: IO ()
main = do
  gen <- getStdGen
  let rs = randomRs (0, 39) gen :: [Int]
  traverse_ print (take 10 rs)
