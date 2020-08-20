{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Wordcrab.Client
  ( Backend(..)
  , State(..)
  , current
  , preview
  , boardCursor
  , rackCursor
  , messages
  , Preview(..)
  , gameState
  , placed
  , displayBoard
  , playResult
  , placeOrSelectTile
  , confirmPlay
  , message
  , updateBlank
  , pickUpTile
  , moveUp
  , moveRight
  , moveDown
  , moveLeft
) where

import Control.Category ((>>>))
import Control.Lens ((?~), to, (%~), (.~), makeLenses, (^.))
import Data.Either (fromRight)
import Data.Function ((&))
import Data.Functor.Identity (Identity(..))
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust, isNothing)
import Data.Text (Text, pack)
import qualified Data.Vector as V

import Wordcrab.Board (Board)
import qualified Wordcrab.Board as Board
import qualified Wordcrab.GameState as GameState
import Wordcrab.GameState (toPreviewState, tiles, currentPlayer, players, GameState(..), board)
import qualified Wordcrab.Player as Player
import Wordcrab.Player (rack)
import Wordcrab.PlayResult (newBoard, PlayResult)
import qualified Wordcrab.PlayResult as PlayResult
import qualified Wordcrab.Tiles as Tiles
import qualified Data.List.NonEmpty as NE
import Data.Bifunctor (first, second)

data Backend m = Backend
  { backendPlay :: BackendPlay m
  , backendPreview :: BackendPlay m
  }

type BackendPlay m =
  Board.Position ->
  Board.Direction ->
  NonEmpty Tiles.PlayedTile ->
  Board.Board Tiles.PlayedTile ->
  m (Either (Board.PlayError Tiles.PlayedTile) PlayResult)

data State = State
  { _current :: GameState Identity
  , _preview :: Preview
  , _boardCursor :: (Int, Int)
  , _rackCursor :: Maybe Int
  , _messages :: [Text]
  }

data Preview = Preview
  { _gameState :: GameState (Either (Board.PlayError Tiles.PlayedTile))
  , _placed :: Map.Map (Int, Int) Tiles.PlayedTile
  , _displayBoard :: Board Tiles.PlayedTile
  , _playResult :: Maybe PlayResult
  }

makeLenses ''State
makeLenses ''Preview

placeOrSelectTile :: Monad m => Backend m -> State -> m State
placeOrSelectTile backend cs =
  if cursorOnBoard
  then if spaceFree
    then pure (cs & rackCursor ?~ 0)
    else pure (message cs "Can't place on another tile")
  else fromRight (pure cs) (placeTile backend cs)
  where
    cursorOnBoard = cs ^. rackCursor . to isNothing
    spaceFree = isNothing cell
    (x, y) = cs ^. boardCursor
    vector = Board.unBoard (cs ^. preview . displayBoard)
    row = Board.unRow (vector V.! y)
    cell = Board.squareContents $ row V.! x

-- | TODO: Prevent playing over a tile you just played
placeTile :: Monad m => Backend m -> State -> Either PlaceError (m State)
placeTile backend cs = do
  i <- note NoCursor $ cs ^. rackCursor
  let playedTile = case (cs ^. preview . gameState . players . currentPlayer . rack) !! i of
        Tiles.Letter lt -> Tiles.PlayedLetter lt
        Tiles.Blank -> Tiles.PlayedBlank '_'
  cs <- pure $ cs & (rackCursor .~ Nothing)
    & preview . placed %~ Map.insert (cs ^. boardCursor) playedTile
    & preview . gameState . players . currentPlayer . rack %~ remove i
  pure $ do
    r <- updatePreview (backendPreview backend) cs
    pure $ case r of
      (Nothing, cs) -> cs
      (Just e, cs) -> cs
        & flip message ("can't play: invalid move (" <> pack (show e) <> ")")

confirmPlay :: Monad m => Backend m -> State -> m State
confirmPlay b cs = do
  (error, newState) <- updatePreview (backendPlay b) cs
  pure $ case error of
    Just e -> message cs $ "Can’t play: invalid move (" <> pack (show e) <> ")"
    Nothing -> let
      newBoard = newState ^. preview . gameState . board
      newScore = newState ^. preview . gameState . players . currentPlayer . Player.score
      remainingTiles = newState ^. preview . gameState . players . currentPlayer . rack
      needed = 7 - length remainingTiles
      (newTiles, newBag) = splitAt needed (newState ^. preview . gameState . tiles)
      in case newBoard of
        Right b -> newState
                  & current . players . currentPlayer . Player.score .~ newScore
                  & current . board .~ Identity b
                  & current . players . currentPlayer . rack .~ (remainingTiles <> newTiles)
                  & current . players %~ GameState.nextPlayer
                  & current . tiles .~ newBag
                  & preview . placed .~ Map.empty
                  & \cs' -> cs' & preview . gameState .~ toPreviewState (cs' ^. current)
        Left e -> message cs $ "Can’t play: invalid move (" <> pack (show e) <> ")"

message :: State -> Text -> State
message cs m = cs & messages %~ (m :)

updateBlank :: Monad m => Backend m -> State -> Char -> m State
updateBlank backend s c = let
  cursor = s ^. boardCursor
  target = Map.lookup cursor (s ^. preview . placed)
  in case (s ^. rackCursor, target) of
    (Nothing, Just (Tiles.PlayedBlank _)) ->
      fmap snd $ updatePreview (backendPreview backend) $
        s & preview . placed %~ Map.adjust (const $ Tiles.PlayedBlank c) cursor
    _ -> pure $ message s "Can only add a letter to a blank tile you've placed this turn"

updatePreview :: Monad m => BackendPlay m -> State -> m (Maybe OrganiseError, State)
updatePreview backendPlay cs = do
  cs <- pure $ cs
          & preview . gameState . board .~ (cs ^. current . board . to runIdentity . to pure)
          & preview . gameState . players . currentPlayer . Player.score
            .~ (cs ^. current . players . currentPlayer . Player.score)
  let ot = organiseTiles cs
  cs <- case ot of
    Left NoTiles -> pure cs
    Left InconsistentDirection -> pure cs
    Right (p, d, ts) -> do
      m <- backendPlay p d ts (runIdentity $ cs ^. current . board)
      pure $ case m of
        Right pr ->
          cs & preview . gameState . board .~ Right (pr ^. newBoard)
             & preview . gameState . players . currentPlayer . Player.score
               .~ ((pr ^. PlayResult.score)
                   + (cs ^. current . players . currentPlayer . Player.score))
             & preview . playResult .~ Just pr
        Left e -> cs & preview . gameState . board .~ Left e
                     & preview . playResult .~ Nothing
  let db = foldr (\(xy, t) b -> updateBoard xy t b)
             (cs ^. current . board . to runIdentity)
             (Map.toList $ cs ^. preview . placed)
  pure (either Just (const Nothing) ot, cs & preview . displayBoard .~ db)

pickUpTile :: Monad m => Backend m -> State -> Either String (m State)
pickUpTile backend cs = do
  _ <- maybe (Right ()) (const $ Left "Can’t delete from rack") (cs ^. rackCursor)
  let m = Map.lookup (cs ^. boardCursor) (cs ^. preview . placed)
  case m of
    Nothing -> Left "Can’t delete a tile you didn’t place this turn"
    Just t -> pure $ fmap snd $
        updatePreview (backendPreview backend) $
          cs & preview . placed %~ Map.delete (cs ^. boardCursor)
             & preview . gameState . players . currentPlayer . rack %~ (Tiles.unplay t :)

note :: a -> Maybe b -> Either a b
note a = maybe (Left a) Right

data PlaceError = Organise OrganiseError | NoCursor

organiseTiles :: State -> Either OrganiseError (Board.Position, Board.Direction, NonEmpty Tiles.PlayedTile)
organiseTiles cs = case Map.toList $ cs ^. preview . placed of
  [] -> Left NoTiles
  t@((x, y), _) : ts -> let
    sorted = NE.sortWith (\(coords, _) -> f coords) (t :| ts)
    horizontal = all ((== y) . snd . fst) ts
    vertical = all ((== x) . fst . fst) ts
    f = case (horizontal, vertical) of
      (True, False) -> fst
      _ -> snd
    direction = case (horizontal, vertical) of
      (True, False) -> Right Board.Horizontal
      (False, True) -> Right Board.Vertical
      (False, False) -> Left InconsistentDirection
      (True, True) -> Right $ case hasHorizontalNeighbours (cs ^. current . board . to runIdentity) (Board.Position x y) of
        Just True -> Board.Horizontal
        _ -> Board.Vertical
    position = uncurry Board.Position $ fst $ NE.head sorted
    in do
      d <- direction
      pure (position, d, fmap snd sorted)

hasHorizontalNeighbours :: Board.Board t -> Board.Position -> Maybe Bool
hasHorizontalNeighbours b p = do
  vp <- Board.validatePosition p b
  let ns = NE.toList (Board.horizontalNeighbours vp b)
  pure $ any (Board.squareContents >>> isJust) ns

data OrganiseError = NoTiles | InconsistentDirection deriving Show

remove :: Int -> [a] -> [a]
remove i xs = take i xs <> drop (i + 1) xs

move ::
  ((Int -> Int) -> (Int, Int) -> (Int, Int)) ->
  (Int -> Int) ->
  State ->
  State
move f g cs = let
  boardMove = f ((`mod` 15) . g) (cs ^. boardCursor)
  rackMove = fmap ((`mod` (length $ cs ^. preview . gameState . players . currentPlayer . rack)) . g)
             (cs ^. rackCursor)
  in if isJust (cs ^. rackCursor)
     then rackCursor .~ rackMove $ cs
     else boardCursor .~ boardMove $ cs

moveRight :: State -> State
moveRight = move first (+ 1)

moveLeft :: State -> State
moveLeft = move first (subtract 1)

moveUp :: State -> State
moveUp = move second (subtract 1)

moveDown :: State -> State
moveDown = move second (+ 1)

toBoardPosition :: (Int, Int) -> Board.Position
toBoardPosition (x, y) = Board.Position x y

updateBoard ::
  (Int, Int) ->
  Tiles.PlayedTile ->
  Board.Board Tiles.PlayedTile ->
  Board.Board Tiles.PlayedTile
updateBoard (x, y) t b = let
  v = Board.unBoard b
  r = Board.unRow $ v V.! y
  s = r V.! x
  r' = r V.// [(x, s { Board.squareContents = Just t })]
  in Board.Board $ v V.// [(y, Board.Row r')]

unupdateBoard
  :: (Int, Int)
  -> Board.Board Tiles.PlayedTile
  -> Board.Board Tiles.PlayedTile
unupdateBoard (x, y) b = let
  v = Board.unBoard b
  r = Board.unRow $ v V.! y
  s = r V.! x
  r' = r V.// [(x, s { Board.squareContents = Nothing })]
  in Board.Board $ v V.// [(y, Board.Row r')]
