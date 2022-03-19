{-# LANGUAGE OverloadedStrings #-}
module Game.Definition where

import qualified Data.Set as S
import qualified Data.Map.Strict as M

import Game.Action
import Game.State
import Game.Skill

simpleGame :: GameDefinition
simpleGame =
  GameDefinition
    { actions =
        M.fromList
          [ (ActionName "Wake up" 0, Action (S.singleton Manipulation) "Open your eyes." NoCondition 10),
            (ActionName "Open door" 0, Action (S.singleton Manipulation) "Turn the handle." (ActionCondition (ActionName "Wake up" 0)) 10)
          ],
      initial_skills = S.fromList [ Walking, Talking, Manipulation ]
    }
