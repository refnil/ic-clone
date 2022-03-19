module Game.Action where

import Data.Text
import qualified Data.Set as S

import Game.Skill

data ActionName = ActionName Text Int
  deriving (Eq, Ord, Show)

data Action = Action
  { skillsUsed :: S.Set Skill,
    message :: Text,
    condition :: Condition,
    cost :: Float
  }
  deriving Show

data Condition
  = NoCondition
  | ActionCondition ActionName
  | OrCondition [Condition]
  | AndCondition [Condition]
  | NotCondition Condition
    deriving Show
