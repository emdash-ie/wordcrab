{-# LANGUAGE DeriveGeneric #-}

module Wordcrab.Player.Id where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)

newtype Id = Id Integer deriving (Eq, Show, Generic)
instance ToJSON Id
instance FromJSON Id

firstId :: Id
firstId = Id 1

nextId :: Id -> Id
nextId (Id i) = Id (i + 1)
