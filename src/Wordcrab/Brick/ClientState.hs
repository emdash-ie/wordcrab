{-# LANGUAGE TemplateHaskell #-}
module Wordcrab.Brick.ClientState where

import Control.Lens (makeLenses, (%~))
import Data.Functor.Identity (Identity(..))
import qualified Data.Map.Strict as Map
import Data.Text (Text)

import Wordcrab.Board (Board)
import qualified Wordcrab.Board as Board
import Wordcrab.GameState (GameState(..))
import qualified Wordcrab.Tiles as Tiles

data ClientState = ClientState
  { _current :: GameState Identity
  , _preview :: PreviewState
  , _boardCursor :: (Int, Int)
  , _rackCursor :: Maybe Int
  , _messages :: [Text]
  }


data PreviewState = PreviewState
  { _gameState :: GameState (Either (Board.PlayError Tiles.PlayedTile))
  , _placed :: Map.Map (Int, Int) Tiles.PlayedTile
  , _displayBoard :: Board Tiles.PlayedTile
  }

makeLenses ''ClientState
makeLenses ''PreviewState
