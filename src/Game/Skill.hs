{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}

module Game.Skill where

import Data.Aeson
import Deriving.Aeson.Stock
import GHC.Generics

data Skill
  = Sword
  | Bow
  | Axe
  | Lance
  | Fight
  | Kata
  | Walking
  | Talking
  | Manipulation
  deriving (Eq, Ord, Show, Generic, FromJSONKey, ToJSONKey)
  deriving (FromJSON, ToJSON) via Snake Skill
