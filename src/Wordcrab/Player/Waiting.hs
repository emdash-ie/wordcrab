{-# LANGUAGE DeriveGeneric #-}

module Wordcrab.Player.Waiting where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)

import Wordcrab.Player.Id

data Waiting = Waiting
  { _id :: Id
  }
  deriving (Eq, Generic, Show)
instance ToJSON Waiting
instance FromJSON Waiting
