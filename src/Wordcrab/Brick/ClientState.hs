{-# LANGUAGE TemplateHaskell #-}
module Wordcrab.Brick.ClientState where

import Control.Lens (makeLenses)
import Data.Functor.Identity (Identity)

import Wordcrab.Board (Board)
import qualified Wordcrab.Board as Board
import qualified Wordcrab.Tiles as Tiles
import Wordcrab.Player (Player(..))

data ClientState = ClientState
  { _current :: GameState Identity
  , _preview :: PreviewState
  , _boardCursor :: (Int, Int)
  , _rackCursor :: Maybe Int
  }


data GameState m = GameState
  { _board :: m (Board Tiles.PlayedTile)
  , _player :: Player
  , _tiles :: [Tiles.Tile]
  }

data PreviewState = PreviewState
  { _gameState :: GameState (Either (Board.PlayError Tiles.PlayedTile))
  , _placed :: [(Tiles.PlayedTile, (Int, Int))]
  , _displayBoard :: Board Tiles.PlayedTile
  }

makeLenses ''ClientState
makeLenses ''GameState
makeLenses ''PreviewState
