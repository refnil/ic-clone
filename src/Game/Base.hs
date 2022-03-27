{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Game.Base where

import Data.Aeson
import Data.Fixed
import qualified Data.Set as S
import qualified Data.Time.Clock as C
import Deriving.Aeson.Stock
import GHC.Float
import GHC.Generics

newtype Time = Time C.NominalDiffTime
  deriving (Show)
  deriving newtype (Eq, Ord, Num, Fractional, FromJSON, ToJSON)

seconds :: Integer -> Time
seconds = Time . fromInteger

secondsF :: Float -> Time
secondsF = realToFrac

newtype Life = Life Float
  deriving (Show)
  deriving newtype (Eq, Ord, Num, FromJSON, ToJSON)

type Alive = Bool

data Experience = Experience
  { hardWork :: Float,
    talent :: Float,
    hardWorkLevel :: Int,
    talentLevel :: Int
  }
  deriving (Show, Generic)
  deriving (FromJSON, ToJSON) via Snake Experience

healthNeeded :: Time -> Time -> Life
healthNeeded start duration = Life 0

toSeconds :: Time -> Int
toSeconds (Time diff) =
  let MkFixed integer = C.nominalDiffTimeToSeconds diff
   in fromInteger integer `div` 1000000000000

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
