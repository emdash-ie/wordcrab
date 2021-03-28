{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Wordcrab.Client (
  Backend (..),
  BackendError (..),
  State (..),
  _Started,
  _Waiting,
  InProgress (..),
  current,
  preview,
  boardCursor,
  rackCursor,
  messages,
  Preview (..),
  gameState,
  placed,
  displayBoard,
  playResult,
  placeOrSelectTile,
  confirmPlay,
  message,
  updateBlank,
  pickUpTile,
  moveUp,
  moveRight,
  moveDown,
  moveLeft,
) where

import Control.Category ((>>>))
import Control.Lens (makeLenses, makePrisms, to, (%~), (.~), (?~), (^.))
import Data.Aeson (FromJSON, ToJSON)
import Data.Bifunctor (first, second)
import Data.Either (fromRight)
import Data.Function ((&))
import Data.Functor.Identity (Identity (..))
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust, isNothing)
import Data.Text (Text, pack)
import qualified Data.Vector as V
import GHC.Generics (Generic)

import Data.Functor ((<&>))
import Data.Void (Void, absurd)
import Wordcrab.API (StartGameResponse)
import qualified Wordcrab.API as API
import Wordcrab.Board (Board)
import qualified Wordcrab.Board as Board
import Wordcrab.GameState (
  GameState (..),
  JoinError,
  StartError,
  board,
  players,
  tiles,
  toPreviewState,
 )
import qualified Wordcrab.GameState as GameState
import Wordcrab.GameState.Event (currentPlayer)
import qualified Wordcrab.GameState.Event as GameState.Event
import Wordcrab.PlayResult (PlayResult, newBoard)
import qualified Wordcrab.PlayResult as PlayResult
import Wordcrab.Player (rack)
import qualified Wordcrab.Player as Player
import Wordcrab.Room (Room)
import qualified Wordcrab.Tiles as Tiles

data Backend m = Backend
  { backendPlay :: Player.Id -> BackendPlay m
  , backendPreview :: BackendPlay m
  , backendJoin :: m (Either (BackendError JoinError) (Room, Player.Id))
  , backendRefresh :: m (Either (BackendError ()) (Maybe (GameState Identity)))
  , backendStart :: m (Either (BackendError Void) (StartGameResponse StartError))
  }

data BackendError e = SpecificError e | OtherError Text deriving (Show, Generic)
instance ToJSON e => ToJSON (BackendError e)
instance FromJSON e => FromJSON (BackendError e)

type BackendPlay m =
  Board.Position ->
  Board.Direction ->
  NonEmpty Tiles.PlayedTile ->
  Board.Board Tiles.PlayedTile ->
  m (Either (BackendError Void) (API.PlayResponse API.PlayError))

data State
  = Waiting Room
  | Started InProgress

data InProgress = InProgress
  { _room :: Room
  , _current :: GameState Identity
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

makePrisms ''State
makeLenses ''Preview
makeLenses ''InProgress

placeOrSelectTile :: Monad m => Backend m -> InProgress -> m InProgress
placeOrSelectTile backend cs =
  if cursorOnBoard
    then
      if spaceFree
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
placeTile :: Monad m => Backend m -> InProgress -> Either PlaceError (m InProgress)
placeTile backend cs = do
  i <- note NoCursor $ cs ^. rackCursor
  let playedTile = case (cs ^. preview . gameState . players . currentPlayer . rack) !! i of
        Tiles.Letter lt -> Tiles.PlayedLetter lt
        Tiles.Blank -> Tiles.PlayedBlank '_'
  cs <-
    pure $
      cs & (rackCursor .~ Nothing)
        & preview . placed %~ Map.insert (cs ^. boardCursor) playedTile
        & preview . gameState . players . currentPlayer . rack %~ remove i
  pure $ do
    r <- updatePreview (backendPreview backend) cs
    pure $ case r of
      (Nothing, cs) -> cs
      (Just e, cs) ->
        cs
          & flip message ("can't play: invalid move (" <> pack (show e) <> ")")

confirmPlay :: Monad m => Backend m -> InProgress -> m InProgress
confirmPlay b cs = do
  let playerID = cs ^. current . players . currentPlayer . Player.id
  (error, newState) <- updatePreview (backendPlay b playerID) cs
  pure $ case error of
    Just e -> message cs $ "Can’t play: invalid move (" <> pack (show e) <> ")"
    Nothing ->
      let newBoard = newState ^. preview . gameState . board
          newScore = newState ^. preview . gameState . players . currentPlayer . Player.score
          remainingTiles = newState ^. preview . gameState . players . currentPlayer . rack
          needed = 7 - length remainingTiles
          (newTiles, newBag) = splitAt needed (newState ^. preview . gameState . tiles)
       in case newBoard of
            Right b ->
              newState
                & current . players . currentPlayer . Player.score .~ newScore
                & current . board .~ Identity b
                & current . players . currentPlayer . rack .~ (remainingTiles <> newTiles)
                & current . players %~ GameState.Event.nextPlayer
                & current . tiles .~ newBag
                & preview . placed .~ Map.empty
                & \cs' -> cs' & preview . gameState .~ toPreviewState (cs' ^. current)
            Left e -> message cs $ "Can’t play: invalid move (" <> pack (show e) <> ")"

message :: InProgress -> Text -> InProgress
message cs m = cs & messages %~ (m :)

updateBlank :: Monad m => Backend m -> InProgress -> Char -> m InProgress
updateBlank backend s c =
  let cursor = s ^. boardCursor
      target = Map.lookup cursor (s ^. preview . placed)
   in case (s ^. rackCursor, target) of
        (Nothing, Just (Tiles.PlayedBlank _)) ->
          fmap snd $
            updatePreview (backendPreview backend) $
              s & preview . placed %~ Map.adjust (const $ Tiles.PlayedBlank c) cursor
        _ -> pure $ message s "Can only add a letter to a blank tile you've placed this turn"

updatePreview :: Monad m => BackendPlay m -> InProgress -> m (Maybe OrganiseError, InProgress)
updatePreview backendPlay cs = do
  cs <-
    pure $
      cs
        & preview . gameState . board .~ (cs ^. current . board . to runIdentity . to pure)
        & preview . gameState . players . currentPlayer . Player.score
          .~ (cs ^. current . players . currentPlayer . Player.score)
  let ot = organiseTiles cs
  cs <- case ot of
    Left NoTiles -> pure cs
    Left InconsistentDirection -> pure cs
    Right (p, d, ts) -> do
      backendPlay p d ts (runIdentity $ cs ^. current . board) <&> \case
        Left (OtherError e) -> message cs ("Something went wrong: " <> e)
        Left (SpecificError e) -> absurd e
        Right (API.PlaySuccessful pr) ->
          cs & preview . gameState . board .~ Right (pr ^. newBoard)
            & preview . gameState . players . currentPlayer . Player.score
              .~ ( (pr ^. PlayResult.score)
                    + (cs ^. current . players . currentPlayer . Player.score)
                 )
            & preview . playResult .~ Just pr
        Right (API.PlayUnsuccessful API.NoGameInProgress) ->
          message cs "There is no game in progress"
        Right (API.PlayUnsuccessful (API.PlayError e)) ->
          cs & preview . gameState . board .~ Left e
            & preview . playResult .~ Nothing
  let db =
        foldr
          (\(xy, t) b -> updateBoard xy t b)
          (cs ^. current . board . to runIdentity)
          (Map.toList $ cs ^. preview . placed)
  pure (either Just (const Nothing) ot, cs & preview . displayBoard .~ db)

pickUpTile :: Monad m => Backend m -> InProgress -> Either String (m InProgress)
pickUpTile backend cs = do
  _ <- maybe (Right ()) (const $ Left "Can’t delete from rack") (cs ^. rackCursor)
  let m = Map.lookup (cs ^. boardCursor) (cs ^. preview . placed)
  case m of
    Nothing -> Left "Can’t delete a tile you didn’t place this turn"
    Just t ->
      pure $
        fmap snd $
          updatePreview (backendPreview backend) $
            cs & preview . placed %~ Map.delete (cs ^. boardCursor)
              & preview . gameState . players . currentPlayer . rack %~ (Tiles.unplay t :)

note :: a -> Maybe b -> Either a b
note a = maybe (Left a) Right

data PlaceError = Organise OrganiseError | NoCursor

organiseTiles :: InProgress -> Either OrganiseError (Board.Position, Board.Direction, NonEmpty Tiles.PlayedTile)
organiseTiles cs = case Map.toList $ cs ^. preview . placed of
  [] -> Left NoTiles
  t@((x, y), _) : ts ->
    let sorted = NE.sortWith (\(coords, _) -> f coords) (t :| ts)
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

data OrganiseError = NoTiles | InconsistentDirection deriving (Show)

remove :: Int -> [a] -> [a]
remove i xs = take i xs <> drop (i + 1) xs

move ::
  ((Int -> Int) -> (Int, Int) -> (Int, Int)) ->
  (Int -> Int) ->
  InProgress ->
  InProgress
move f g cs =
  let boardMove = f ((`mod` 15) . g) (cs ^. boardCursor)
      rackMove =
        fmap
          ((`mod` (length $ cs ^. preview . gameState . players . currentPlayer . rack)) . g)
          (cs ^. rackCursor)
   in if isJust (cs ^. rackCursor)
        then rackCursor .~ rackMove $ cs
        else boardCursor .~ boardMove $ cs

moveRight :: InProgress -> InProgress
moveRight = move first (+ 1)

moveLeft :: InProgress -> InProgress
moveLeft = move first (subtract 1)

moveUp :: InProgress -> InProgress
moveUp = move second (subtract 1)

moveDown :: InProgress -> InProgress
moveDown = move second (+ 1)

toBoardPosition :: (Int, Int) -> Board.Position
toBoardPosition (x, y) = Board.Position x y

updateBoard ::
  (Int, Int) ->
  Tiles.PlayedTile ->
  Board.Board Tiles.PlayedTile ->
  Board.Board Tiles.PlayedTile
updateBoard (x, y) t b =
  let v = Board.unBoard b
      r = Board.unRow $ v V.! y
      s = r V.! x
      r' = r V.// [(x, s{Board.squareContents = Just t})]
   in Board.Board $ v V.// [(y, Board.Row r')]

unupdateBoard ::
  (Int, Int) ->
  Board.Board Tiles.PlayedTile ->
  Board.Board Tiles.PlayedTile
unupdateBoard (x, y) b =
  let v = Board.unBoard b
      r = Board.unRow $ v V.! y
      s = r V.! x
      r' = r V.// [(x, s{Board.squareContents = Nothing})]
   in Board.Board $ v V.// [(y, Board.Row r')]
