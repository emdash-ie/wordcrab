{-# LANGUAGE LambdaCase #-}

module Wordcrab.Test where

import Control.Category ((>>>))
import Control.Monad (guard)
import Data.Char (toLower, toUpper)
import Data.List (lookup, (\\))
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Maybe (listToMaybe)
import System.Random (getStdGen)
import Text.Read (readMaybe)

import Wordcrab.Board (Board)
import qualified Wordcrab.Board as Board
import Wordcrab.Player (Player (..))
import qualified Wordcrab.Tiles as Tiles

badUX :: IO ()
badUX = do
  gen <- getStdGen
  let board = Board.blankBoard
  putStrLn (Board.showBoard Tiles.showTile board)
  let (startingRack, tiles) = splitAt 7 (Tiles.shuffleBag gen Tiles.tileset)
  let player = Player{_rack = startingRack, _score = 0}
  turns board tiles player

turns :: Board Tiles.PlayedTile -> [Tiles.Tile] -> Player -> IO ()
turns board tiles player = do
  result <- turn board tiles player
  printGameState result
  case result of
    Left _ -> putStrLn "There was a problem with the last turn. Exiting."
    Right (board', tiles', player') -> turns board' tiles' player'

turn ::
  Board Tiles.PlayedTile ->
  [Tiles.Tile] ->
  Player ->
  IO (Either (Board.PlayError Tiles.PlayedTile) (Board Tiles.PlayedTile, [Tiles.Tile], Player))
turn b ts p = do
  position <- getPositionFromUser
  -- pure $ guard (positionIsClear position)
  direction <- getDirectionFromUser
  playedTiles <- getTilesFromUser (NE.fromList $ _rack p)
  pure $ do
    ((b', _, _), s) <- Board.play position direction playedTiles Tiles.tileScore b
    let (newTiles, rest) = splitAt (length playedTiles) ts
    let p' =
          p
            { _rack = (_rack p \\ NE.toList (fmap fromPlayedTile playedTiles)) <> newTiles
            , _score = _score p + s
            }
    pure (b', rest, p')

printGameState :: Either (Board.PlayError Tiles.PlayedTile) (Board Tiles.PlayedTile, [Tiles.Tile], Player) -> IO ()
printGameState (Left _) = putStrLn "Error in play! (Please start again D:)"
printGameState (Right (b, _, p)) = do
  putStrLn (Board.showBoard Tiles.showTile b)
  putStrLn $ "Score: " <> show (_score p)

-- putStrLn $ "Rack: " <> showRack (rack p)

{- | Should probably be a maybe or something, if we want the user to be able to
 quit at any point.
-}
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
  -- putStrLn $ "(Your tiles are: " <> showRack (NE.toList ts) <> ")"
  line <- getLine
  case (readMaybe line :: Maybe [String]) of
    Nothing -> retry "Sorry, I don't understand."
    Just [] -> retry "Please choose at least 1 tile."
    Just ts' ->
      let (matches, unmatched) = readTileSelection ts (NE.fromList ts')
       in case unmatched of
            [] -> maybe (retry "Sorry, no matches.") pure (NE.nonEmpty matches)
            xs ->
              retry $
                unlines $
                  ["Sorry, I couldn't understand the following:"] <> xs
 where
  retry s = putStrLn s >> getTilesFromUser ts

readTileSelection :: NonEmpty Tiles.Tile -> NonEmpty String -> ([Tiles.PlayedTile], [String])
readTileSelection ts selections =
  let associationList = NE.toList $ fmap (\t -> (tileToString t, t)) ts
      f :: String -> ([Tiles.PlayedTile], [String]) -> ([Tiles.PlayedTile], [String])
      f s (matches, unmatched) =
        let lower = fmap toLower s
            lookedUp = do
              t <- lookup (take 5 lower) associationList
              toPlayedTile t (drop 6 lower)
         in case lookedUp of
              Nothing -> (matches, s : unmatched)
              Just m -> (m : matches, unmatched)
   in foldr f ([], []) (NE.toList selections)

fromPlayedTile :: Tiles.PlayedTile -> Tiles.Tile
fromPlayedTile = \case
  Tiles.PlayedBlank _ -> Tiles.Blank
  Tiles.PlayedLetter lt -> Tiles.Letter lt

toPlayedTile :: Tiles.Tile -> String -> Maybe Tiles.PlayedTile
toPlayedTile (Tiles.Letter lt) _ = Just (Tiles.PlayedLetter lt)
toPlayedTile Tiles.Blank s = do
  c <- listToMaybe s
  pure $ Tiles.PlayedBlank $ toUpper c

tileToString :: Tiles.Tile -> String
tileToString Tiles.Blank = "blank"
tileToString (Tiles.Letter lt) = pure $ toLower $ Tiles.letter lt

matchSelection :: NonEmpty Tiles.Tile -> String -> Maybe ([Tiles.Tile], Tiles.PlayedTile)
matchSelection (t :| ts) s = case matchTile t s of
  Just pt -> Just (ts, pt)
  Nothing -> do
    (ts', pt') <- NE.nonEmpty ts >>= flip matchSelection s
    pure (t : ts', pt')

matchTile :: Tiles.Tile -> String -> Maybe Tiles.PlayedTile
matchTile t s =
  let lower = fmap toLower s
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
testPlayer ts b p =
  let makePlayed = \case
        Tiles.Blank -> Tiles.PlayedBlank 'A'
        Tiles.Letter lt -> Tiles.PlayedLetter lt
      playedTiles = NE.fromList $ makePlayed <$> take 3 (_rack p)
      position = Board.Position 0 0
      ((b', _, _), playScore) = fromRight $ Board.play position Board.Vertical playedTiles Tiles.tileScore b
      (newTiles, rest) = splitAt 3 ts
   in ( b'
      , rest
      , p
          { _rack = _rack p <> newTiles
          , _score = _score p + playScore
          }
      )

showTileInPlay :: Board.TileInPlay Tiles.PlayedTile -> String
showTileInPlay (when, square) =
  let active = case when of
        Board.PlayedNow -> " (*)"
        Board.PlayedEarlier -> ""
   in "- " <> show (Board.squareType square) <> active <> ": "
        <> show (Board.squareContents square)

display :: (Board.Play Tiles.PlayedTile, Integer) -> IO ()
display ((b, mw, pws), s) =
  putStrLn (Board.showBoard Tiles.showTile b)
    >> putStrLn "Main word:"
    >> traverse (showTileInPlay >>> putStrLn) mw
    >> putStrLn "Perp words:"
    >> traverse (fmap showTileInPlay >>> traverse putStrLn) pws
    >> putStrLn ("Score: " <> show s)

testBoard :: IO ()
testBoard =
  let b = Board.blankBoard
      t1@((b1, mw1, pw1), s1) =
        fromRight $
          Board.play
            (Board.Position 7 7)
            Board.Horizontal
            (fmap Tiles.PlayedLetter (Tiles.a :| [Tiles.b]))
            Tiles.tileScore
            b
      t2@((b2, mw2, pw2), s2) =
        fromRight $
          Board.play (Board.Position 6 7) Board.Horizontal (fmap Tiles.PlayedLetter (Tiles.t :| [Tiles.l, Tiles.e])) Tiles.tileScore b1
      t3@((b3, mw3, pw3), s3) =
        fromRight $
          Board.play (Board.Position 9 4) Board.Vertical (fmap Tiles.PlayedLetter (Tiles.h :| [Tiles.e, Tiles.l])) Tiles.tileScore b2
      t4@((b4, mw4, pw4), s4) =
        fromRight $
          Board.play (Board.Position 6 8) Board.Vertical (fmap Tiles.PlayedLetter (Tiles.h :| [Tiles.i, Tiles.n, Tiles.k])) Tiles.tileScore b3
      t5@((b5, mw5, pw5), s5) =
        fromRight $
          Board.play (Board.Position 9 8) Board.Horizontal (fmap Tiles.PlayedLetter (Tiles.o :| [Tiles.x, Tiles.e, Tiles.n])) Tiles.tileScore b4
      fullSequence =
        putStrLn "1:" >> display t1 >> getLine
          >> putStrLn "2:"
          >> display t2
          >> getLine
          >> putStrLn "3:"
          >> display t3
          >> getLine
          >> putStrLn "4:"
          >> display t4
          >> getLine
          >> putStrLn "5:"
          >> display t5
   in fullSequence

fromRight :: Either a b -> b
fromRight (Right b) = b
