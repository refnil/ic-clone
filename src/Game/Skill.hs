{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
module Game.Skill where

import GHC.Generics
import Data.Aeson
import Deriving.Aeson.Stock

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
