module Game.Skill where

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
  deriving (Eq, Ord, Show)
