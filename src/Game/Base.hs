{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Game.Base where

import qualified Data.Set as S
import qualified Data.Time.Clock as C
import GHC.Float
import Data.Fixed

newtype Time = Time C.NominalDiffTime
  deriving (Show)
  deriving newtype (Eq, Ord, Num, Fractional)

seconds :: Integer -> Time
seconds = Time . fromInteger

secondsF :: Float -> Time
secondsF = realToFrac

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

toSecondsF :: Time -> Float
toSecondsF (Time diff) = 
    let MkFixed integer = C.nominalDiffTimeToSeconds diff
     in (fromInteger integer) / (10 ** 12)

computeLife :: Time -> Time -> Life -> (Life, Maybe Time)
computeLife period start (Life currentLife) =
  let periodF = toSecondsF period
      startF = toSecondsF start
      base = 60 * log 1.25 * (1.25 ** (startF / 60))
      lifeLostDuringAction = base * (1.25 ** (periodF / 60) - 1)
      timeBeforeDeath = 60 * logBase 1.25 (currentLife / base + 1)
   in if currentLife > lifeLostDuringAction
        then (Life lifeLostDuringAction, Nothing)
        else (Life lifeLostDuringAction, Just . secondsF $ timeBeforeDeath)

toSpeed :: Experience -> Float
toSpeed exp = 1.05 ** int2Float (hardWorkLevel exp) * 1.01 ** int2Float (talentLevel exp)

resetExperience :: Experience -> Experience
resetExperience exp = exp {hardWork = 0, hardWorkLevel = 0}
