{-# LANGUAGE OverloadedStrings #-}

module Game.Definition where

import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Text
import GHC.Float
import Game.Action
import Game.Skill
import Game.State

allDefinitions :: [GameDefinition]
allDefinitions = [startingHouse, village1]

fullGame :: GameDefinition
fullGame = mconcat allDefinitions

toDef :: [(ActionName, Action)] -> GameDefinition
toDef actions = GameDefinition (M.fromList actions) mempty

an :: Text -> ActionName
an name = ActionName name 0

anc :: Text -> Condition
anc = ActionCondition . an

startingHouse :: GameDefinition
startingHouse =
  GameDefinition
    { actions =
        M.fromList
          [ (an "wake up", Action (S.singleton Manipulation) "Open your eyes." NoCondition 1),
            (an "open door", Action (S.singleton Manipulation) "Turn the handle." (anc "wake up") 2),
            (an "take bow", Action (S.fromList [Manipulation, Bow]) "Turn the handle." (anc "wake up") 20)
          ],
      initial_skills = S.fromList [Walking, Talking, Manipulation]
    }

village1 :: GameDefinition
village1 =
  toDef
    ( [ (an "general store", Action (S.singleton Talking) "Talk to the general store employee." (anc "open door") 5),
        (an "general store forage", Action (S.singleton Talking) "Accept the request to pick 10 flowers in the nearby forest from the store employee." (anc "general store") 10),
        (an "walk forest", Action (S.singleton Walking) "Walking to the forest near the village." (anc "general store forage") 10),
        (an "blacksmith", Action (S.singleton Talking) "Talk to the blackmith." (anc "open door") 15)
      ]
        <> actionSuite "pick flower" 10 (S.fromList [Walking, Manipulation]) "Search and pick a flower." (anc "walk forest") (\i -> 10 + 3 * i * i)
    )

actionSuite :: Text -> Int -> S.Set Skill -> Text -> Condition -> (Int -> Int) -> [(ActionName, Action)]
actionSuite name count skills text condition costGenerator = fmap genAction [0 .. count]
  where
    genAction i = (ActionName name i, Action skills text (if i == 0 then condition else ActionCondition (ActionName name (i -1))) (int2Float $ costGenerator i))
