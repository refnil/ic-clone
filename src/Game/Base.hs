{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Game.Base where
    
import qualified Data.Set as S

import GHC.Float

newtype Time = Time Float
  deriving (Show)
  deriving newtype (Eq, Ord, Num)

newtype Life = Life Float
  deriving (Show)
  deriving newtype (Eq, Ord, Num)

type Alive = Bool

data Experience = Experience
  { hardWork :: Float,
    talent :: Float,
    hardWorkLevel :: Int,
    talentLevel :: Int
  }
  deriving (Show)

healthNeeded :: Time -> Time -> Life
healthNeeded start duration = Life 0

computeLife :: Time -> Time -> Life -> (Life, Maybe Time)
computeLife (Time period) (Time start) (Life currentLife) =
  let base = 60 * log 1.25 * (1.25 ** (start / 60))
      lifeLostDuringAction = base * (1.25 ** (period / 60) - 1)
      timeBeforeDeath = 60 * logBase 1.25 (currentLife / base + 1)
   in if currentLife > lifeLostDuringAction
        then (Life lifeLostDuringAction, Nothing)
        else (Life lifeLostDuringAction, Just $ Time timeBeforeDeath)

toSpeed :: Experience -> Float
toSpeed exp = 1.05 ** int2Float (hardWorkLevel exp) * 1.01 ** int2Float (talentLevel exp)

resetExperience :: Experience -> Experience
resetExperience exp = exp {hardWork = 0, hardWorkLevel = 1}
