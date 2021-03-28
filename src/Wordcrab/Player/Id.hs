{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Wordcrab.Player.Id where

import Data.Aeson (FromJSON, FromJSONKey, ToJSON, ToJSONKey)
import GHC.Generics (Generic)

newtype Id = Id Integer
  deriving stock (Eq, Show, Ord, Generic)
  deriving newtype (ToJSONKey, FromJSONKey)

instance ToJSON Id
instance FromJSON Id

firstId :: Id
firstId = Id 1

nextId :: Id -> Id
nextId (Id i) = Id (i + 1)
