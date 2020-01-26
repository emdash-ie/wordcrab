{-# LANGUAGE RecordWildCards #-}
module NewBoard where

import Control.Category ((>>>))
import Control.Monad ((>=>))
import Data.Bool (bool)
import Data.Foldable (fold)
import Data.Function ((&))
import Data.List (intersperse)
import qualified Data.List.NonEmpty as NE
import Data.List.NonEmpty (NonEmpty(..))
import Data.Maybe (fromJust)
import qualified Data.Vector as V
import Data.Vector (Vector)
import Prelude hiding (lookup)

import qualified Board
import qualified Tiles

newtype Board = Board { unBoard :: Vector Row }
newtype Row = Row { unRow :: Vector (Maybe Tiles.PlayedTile) }

newtype ValidPosition = ValidPosition Board.Position deriving Show

validatePosition :: Board.Position -> Board -> Maybe ValidPosition
validatePosition p (Board rs) = let
  validY = Board.positionY p < fromIntegral (length rs)
  validX = case rs V.!? 0 of
    Nothing -> False
    Just (Row ts) -> Board.positionX p < fromIntegral (length ts)
  in bool Nothing (Just $ ValidPosition p) (validY && validX)

blankBoard :: Integer -> Integer -> Board
blankBoard columns rows = Board $ V.replicate (fromIntegral rows)
                            $ Row $ V.replicate (fromIntegral columns) Nothing

lookup :: Board -> ValidPosition -> Maybe Tiles.PlayedTile
lookup (Board rs) (ValidPosition p) = (unRow (rs V.! Board.positionY p)) V.! Board.positionX p

update :: Board -> ValidPosition -> Tiles.PlayedTile -> Maybe Board
update b p t = case lookup b p of
  Just t -> Nothing
  Nothing -> Just $ write b p t

write :: Board -> ValidPosition -> Tiles.PlayedTile -> Board
write (Board rs) (ValidPosition p) t = let
  oldRow = rs V.! Board.positionY p
  newRow = Row $ (unRow oldRow) V.// [(Board.positionX p, Just t)]
  in Board (rs V.// [(Board.positionY p, newRow)])

writeSeveral :: Foldable a => a (ValidPosition, Tiles.PlayedTile) -> Board -> Board
writeSeveral xs b = foldr (\(p, t) b -> write b p t) b xs

playIndices ::
  Board.Position ->
  Board.Direction ->
  NonEmpty Tiles.PlayedTile ->
  Board ->
  Maybe (NonEmpty (ValidPosition, Tiles.PlayedTile))
playIndices p d ts b = do
  vp <- validatePosition p b
  case lookup b vp of
    Just _ -> playIndices (Board.forward d p) d ts b
    Nothing -> case NE.uncons ts of
        (t, Nothing) -> Just $ (vp, t) :| []
        (t, Just ts') -> NE.cons (vp, t)
                           <$> playIndices (Board.forward d p) d ts' b

wordAt ::
  ValidPosition ->
  Board.Direction ->
  Board ->
  [Tiles.PlayedTile]
wordAt p d b = wordFrom (startOfWord p d b) d b

startOfWord ::
  ValidPosition ->
  Board.Direction ->
  Board ->
  ValidPosition
startOfWord vp@(ValidPosition p) d b =
  case validatePosition (Board.backward d p) b of
    Nothing -> vp
    Just p' -> case lookup b p' of
      Nothing -> vp
      Just _ -> startOfWord p' d b

wordFrom ::
  ValidPosition ->
  Board.Direction ->
  Board ->
  [Tiles.PlayedTile]
wordFrom vp@(ValidPosition p) d b = case lookup b vp of
  Nothing -> []
  Just t -> case validatePosition (Board.forward d p) b of
    Nothing -> [t]
    Just vp' -> t : wordFrom vp' d b

play ::
  Board.Position ->
  Board.Direction ->
  NonEmpty Tiles.PlayedTile ->
  Board ->
  Maybe (Board, NonEmpty Tiles.PlayedTile, [[Tiles.PlayedTile]])
play p d ts b = do
  indices <- playIndices p d ts b
  -- check indices for neighbouring word
  let b' = writeSeveral indices b
  let mainWord = NE.fromList $ wordAt (fst $ NE.head indices) d b'
  let perpWords = NE.filter ((> 1) . length) $ fmap (\(i, _) -> wordAt i (succ d) b') indices
  pure (b', mainWord, perpWords)

testBoard :: IO ()
testBoard = let
  display (b, mw, pws) = putStrLn (showBoard b)
    >> putStrLn "Main word:" >> print mw
    >> putStrLn "Perp words:" >> print pws
  b = blankBoard 10 10
  t1@(b', mw', pw') = fromJust $
    play (Board.Position 2 5) Board.Horizontal (NE.fromList $ Tiles.blanks "ab") b
  t2@(b'', mw'', pw'') = fromJust $
    play (Board.Position 1 5) Board.Horizontal (NE.fromList $ Tiles.blanks "tle") b'
  t3@(b''', mw''', pw''') = fromJust $
    play (Board.Position 4 2) Board.Vertical (NE.fromList $ Tiles.blanks "helo") b''
  t4@(b'''', mw'''', pw'''') = fromJust $
    play (Board.Position 1 6) Board.Vertical (NE.fromList $ Tiles.blanks "hink") b'''
  t5@(b5, mw5, pw5) = fromJust $
    play (Board.Position 5 6) Board.Horizontal (NE.fromList $ Tiles.blanks "xen") b''''
  fullSequence = putStrLn "1:" >> display t1 >> getLine
                 >> putStrLn "2:" >> display t2 >> getLine
                 >> putStrLn "3:" >> display t3 >> getLine
                 >> putStrLn "4:" >> display t4 >> getLine
                 >> putStrLn "5:" >> display t5
  in fullSequence

showBoard :: Board -> String
showBoard (Board rs) = let
  inner = "\n" <> replicate 20 '-' <> "\n"
  in fold . intersperse "\n" . fmap showRow $ V.toList rs

showRow :: Row -> String
showRow (Row ts) = fmap showCell ts & V.toList & intersperse "|" & fold

showCell :: Maybe Tiles.PlayedTile -> String
showCell Nothing = "_"
showCell (Just (Tiles.PlayedBlank c)) = [c]
showCell _ = undefined
