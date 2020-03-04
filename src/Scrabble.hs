{-# LANGUAGE LambdaCase #-}
module Scrabble where

import Control.Category ((>>>))
import Control.Monad (guard)
import Data.Bifunctor (first, second)
import Data.Bool (bool)
import Data.Foldable (fold)
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.List (intersperse)
import qualified Data.List.NonEmpty as NE
import Data.List.NonEmpty (NonEmpty(..))
import Data.Maybe (fromJust, isJust, catMaybes)
import qualified Data.Vector as V
import Data.Vector (Vector)
import Prelude hiding (lookup)
import System.Random (getStdGen)
import System.Random.Shuffle (shuffle')


import qualified Board
import qualified Tiles

main :: IO ()
main = do
  gen <- getStdGen
  let things = "hello there!"
  print $ shuffle' things (length things) gen

testBoard :: IO ()
testBoard = let
  display :: (Board.Play Tiles.PlayedTile, Integer) -> IO ()
  display ((b, mw, pws), s) = putStrLn (Board.showBoard showTile b)
    >> putStrLn "Main word:" >> traverse (showTileInPlay >>> putStrLn) mw
    >> putStrLn "Perp words:" >> traverse (fmap showTileInPlay >>> traverse putStrLn) pws
    >> putStrLn ("Score: " <> show s)
  showTile :: Tiles.PlayedTile -> String
  showTile = \case
    Tiles.PlayedBlank c -> [c]
    Tiles.PlayedLetter lt -> [Tiles.letter lt]
  showTileInPlay :: Board.TileInPlay Tiles.PlayedTile -> String
  showTileInPlay (when, square) = let
    active = case when of Board.PlayedNow -> " (*)"
                          Board.PlayedEarlier -> ""
    in "- " <> show (Board.squareType square) <> active <> ": "
       <> show (Board.squareContents square)
  b = Board.blankBoard
  t1@((b1, mw1, pw1), s1) = fromJust $
    Board.play (Board.Position 7 7) Board.Horizontal
      (fmap Tiles.PlayedLetter (Tiles.a :| [Tiles.b])) Tiles.tileScore b
  t2@((b2, mw2, pw2), s2) = fromJust $
    Board.play (Board.Position 6 7) Board.Horizontal (fmap Tiles.PlayedLetter (Tiles.t :| [Tiles.l, Tiles.e])) Tiles.tileScore b1
  t3@((b3, mw3, pw3), s3) = fromJust $
    Board.play (Board.Position 9 4) Board.Vertical (fmap Tiles.PlayedLetter (Tiles.h :| [Tiles.e, Tiles.l])) Tiles.tileScore b2
  t4@((b4, mw4, pw4), s4) = fromJust $
    Board.play (Board.Position 6 8) Board.Vertical (fmap Tiles.PlayedLetter (Tiles.h :| [Tiles.i, Tiles.n, Tiles.k])) Tiles.tileScore b3
  t5@((b5, mw5, pw5), s5) = fromJust $
    Board.play (Board.Position 9 8) Board.Horizontal (fmap Tiles.PlayedLetter (Tiles.o :| [Tiles.x, Tiles.e, Tiles.n])) Tiles.tileScore b4
  fullSequence = putStrLn "1:" >> display t1 >> getLine
                 >> putStrLn "2:" >> display t2 >> getLine
                 >> putStrLn "3:" >> display t3 >> getLine
                 >> putStrLn "4:" >> display t4 >> getLine
                 >> putStrLn "5:" >> display t5
  in fullSequence
