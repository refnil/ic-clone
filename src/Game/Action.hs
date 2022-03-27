{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}

module Game.Action where

import Data.Aeson
import qualified Data.Set as S
import Data.Text
import Deriving.Aeson.Stock
import Game.Skill

data ActionName = ActionName Text Int
  deriving (Eq, Ord, Show, Generic)
  deriving (ToJSON, FromJSON) via Snake ActionName

data Action = Action
  { skillsUsed :: S.Set Skill,
    message :: Text,
    condition :: Condition,
    cost :: Float
  }
  deriving (Show)

data Condition
  = NoCondition
  | ActionCondition ActionName
  | OrCondition [Condition]
  | AndCondition [Condition]
  | NotCondition Condition
  deriving (Show)
