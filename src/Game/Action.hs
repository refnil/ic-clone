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

data Condition
  = NoCondition
  | ActionCondition ActionName
