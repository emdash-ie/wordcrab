{-# LANGUAGE LambdaCase #-}
module Scrabble where

import Control.Category ((>>>))
import Control.Monad (guard)
import Data.Char (toLower, toUpper)
import Data.List (lookup)
import qualified Data.List.NonEmpty as NE
import Data.List.NonEmpty (NonEmpty(..))
import Data.Maybe (fromJust, listToMaybe)
import Prelude hiding (lookup)
import System.Random (getStdGen, StdGen)
import System.Random.Shuffle (shuffle')
import Text.Read (readMaybe)


import qualified Board
import Board (Board)
import qualified Tiles
import Player (Player(..))

main :: IO ()
main = do
  gen <- getStdGen
  let board = Board.blankBoard
  putStrLn (Board.showBoard showTile board)
  let (startingRack, tiles) = splitAt 7 (shuffleBag gen Tiles.tileset)
  let player = Player { rack = startingRack, score = 0 }
  turn board tiles player >>= printGameState

-- initialise board, tiles, and player
-- show board
-- Turn (repeat):
-- - prompt user for location on board
--   - check location is clear?
-- - prompt user for tiles
--   - make play, check if valid
-- - show resulting score, board, and rack
-- Finish when we're out of tiles, or allow quit command?

showRack :: [Tiles.Tile] -> String
showRack ts = show $ fmap showRackTile ts
  where showRackTile Tiles.Blank = "Blank"
        showRackTile (Tiles.Letter lt) = pure (Tiles.letter lt)

printGameState :: Maybe (Board Tiles.PlayedTile, [Tiles.Tile], Player) -> IO ()
printGameState Nothing = putStrLn "Error in play! (Please start again D:)"
printGameState (Just (b, _, p)) = do
  putStrLn (Board.showBoard showTile b)
  putStrLn $ "Score: " <> show (score p)
  putStrLn $ "Rack: " <> showRack (rack p)

showTile :: Tiles.PlayedTile -> String
showTile = \case
  Tiles.PlayedBlank c -> [c]
  Tiles.PlayedLetter lt -> [Tiles.letter lt]

shuffleBag :: StdGen -> [Tiles.Tile] -> [Tiles.Tile]
shuffleBag gen ts = shuffle' ts (length ts) gen

turn ::
  Board Tiles.PlayedTile ->
  [Tiles.Tile] ->
  Player ->
  IO (Maybe (Board Tiles.PlayedTile, [Tiles.Tile], Player))
turn b ts p = do
  position <- getPositionFromUser
  -- pure $ guard (positionIsClear position)
  direction <- getDirectionFromUser
  playedTiles <- getTilesFromUser (NE.fromList $ rack p)
  pure $ do
    ((b', _, _), s) <- Board.play position direction playedTiles Tiles.tileScore b
    let (newTiles, rest) = splitAt (length playedTiles) ts
    let p' = p { rack = rack p <> newTiles
                          , score = score p + s
                          }
    pure (b', rest, p')

-- | Should probably be a maybe or something, if we want the user to be able to
-- quit at any point.
getPositionFromUser :: IO Board.Position
getPositionFromUser = do
  putStrLn "Enter a position (x, y) (e.g. (4, 5)):"
  line <- getLine
  case (readMaybe line :: Maybe (Int, Int)) of
    Nothing -> putStrLn "Sorry, I don't understand." >> getPositionFromUser
    Just (x, y) -> pure $ Board.Position x y

getDirectionFromUser :: IO Board.Direction
getDirectionFromUser = do
  putStrLn "Enter a direction, either \"Horizontal\" or \"Vertical\":"
  line <- getLine
  case fmap toLower line of
    "vertical" -> pure Board.Vertical
    "horizontal" -> pure Board.Horizontal
    _ -> putStrLn "Sorry, I don't understand." >> getDirectionFromUser

getTilesFromUser :: NonEmpty Tiles.Tile -> IO (NonEmpty Tiles.PlayedTile)
getTilesFromUser ts = do
  putStrLn "Choose some tiles from the list (e.g. [\"B\", \"A\", \"Blank D\"]):"
  putStrLn $ "(Your tiles are: " <> showRack (NE.toList ts) <> ")"
  line <- getLine
  case (readMaybe line :: Maybe [String]) of
    Nothing -> retry "Sorry, I don't understand."
    Just [] -> retry "Please choose at least 1 tile."
    Just ts' -> let
      (matches, unmatched) = readTileSelection ts (NE.fromList ts')
      in case unmatched of
        [] -> maybe (retry "Sorry, no matches.") pure (NE.nonEmpty matches)
        xs -> retry $ unlines $
         ["Sorry, I couldn't understand the following:"] <> xs
  where retry s = putStrLn s >> getTilesFromUser ts

readTileSelection :: NonEmpty Tiles.Tile -> NonEmpty String -> ([Tiles.PlayedTile], [String])
readTileSelection ts selections = let
  associationList = NE.toList $ fmap (\t -> (tileToString t, t)) ts
  f :: String -> ([Tiles.PlayedTile], [String]) -> ([Tiles.PlayedTile], [String])
  f s (matches, unmatched) = let
    lower = fmap toLower s
    lookedUp = do
      t <- lookup (take 5 lower) associationList
      toPlayedTile t (drop 6 lower)
    in case lookedUp of
      Nothing -> (matches, s : unmatched)
      Just m -> (m : matches, unmatched)
  in foldr f ([], []) (NE.toList selections)

tileToString :: Tiles.Tile -> String
tileToString Tiles.Blank = "blank"
tileToString (Tiles.Letter lt) = pure $ toLower $ Tiles.letter lt

toPlayedTile :: Tiles.Tile -> String -> Maybe Tiles.PlayedTile
toPlayedTile (Tiles.Letter lt) _ = Just (Tiles.PlayedLetter lt)
toPlayedTile Tiles.Blank s = do
  c <- listToMaybe s
  pure $ Tiles.PlayedBlank $ toUpper c

matchSelection :: NonEmpty Tiles.Tile -> String -> Maybe ([Tiles.Tile], Tiles.PlayedTile)
matchSelection (t :| ts) s = case matchTile t s of
  Just pt -> Just (ts, pt)
  Nothing -> do
    (ts', pt') <- NE.nonEmpty ts >>= flip matchSelection s
    pure (t : ts', pt')

matchTile :: Tiles.Tile -> String -> Maybe Tiles.PlayedTile
matchTile t s = let
  lower = fmap toLower s
  in case t of
  Tiles.Blank -> do
    guard (take 5 lower == "blank")
    Tiles.PlayedBlank . toUpper <$> listToMaybe (drop 6 lower)
  Tiles.Letter lt -> do
    c <- listToMaybe s
    guard (c == Tiles.letter lt)
    pure $ Tiles.PlayedLetter lt

testPlayer ::
  [Tiles.Tile] ->
  Board Tiles.PlayedTile ->
  Player ->
  (Board Tiles.PlayedTile, [Tiles.Tile], Player)
testPlayer ts b p = let
  makePlayed = \case Tiles.Blank -> Tiles.PlayedBlank 'A'
                     Tiles.Letter lt -> Tiles.PlayedLetter lt
  playedTiles = NE.fromList $ makePlayed <$> take 3 (rack p)
  position = Board.Position 0 0
  ((b', _, _), playScore) = fromJust $ Board.play position Board.Vertical playedTiles Tiles.tileScore b
  (newTiles, rest) = splitAt 3 ts
  in (b', rest, p { rack = rack p <> newTiles
                  , score = score p + playScore })

showTileInPlay :: Board.TileInPlay Tiles.PlayedTile -> String
showTileInPlay (when, square) = let
  active = case when of Board.PlayedNow -> " (*)"
                        Board.PlayedEarlier -> ""
  in "- " <> show (Board.squareType square) <> active <> ": "
      <> show (Board.squareContents square)

display :: (Board.Play Tiles.PlayedTile, Integer) -> IO ()
display ((b, mw, pws), s) = putStrLn (Board.showBoard showTile b)
  >> putStrLn "Main word:" >> traverse (showTileInPlay >>> putStrLn) mw
  >> putStrLn "Perp words:" >> traverse (fmap showTileInPlay >>> traverse putStrLn) pws
  >> putStrLn ("Score: " <> show s)

testBoard :: IO ()
testBoard = let
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
