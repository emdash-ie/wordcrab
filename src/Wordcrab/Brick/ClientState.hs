{-# LANGUAGE TemplateHaskell #-}
module Wordcrab.Brick.ClientState where

import Control.Lens (makeLenses)

import Wordcrab.Board (Board)
import qualified Wordcrab.Tiles as Tiles
import Wordcrab.Player (Player(..))

data ClientState = ClientState
  { _current :: GameState
  , _preview :: PreviewState
  , _boardCursor :: (Int, Int)
  , _rackCursor :: Maybe Int
  }


data GameState = GameState
  { _board :: Board Tiles.PlayedTile
  , _player :: Player
  , _tiles :: [Tiles.Tile]
  }

data PreviewState = PreviewState
  { _gameState :: GameState
  , _placed :: [(Tiles.PlayedTile, (Int, Int))]
  }

makeLenses ''ClientState
makeLenses ''GameState
makeLenses ''PreviewState
