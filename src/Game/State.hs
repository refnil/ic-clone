module Game.State where

import Control.Monad
import qualified Data.List as L
import qualified Data.Map.Strict as M
import qualified Data.Set as S

import Game.Action
import Game.Skill
import Game.Base

data GameDefinition = GameDefinition
  { actions :: M.Map ActionName Action,
    initial_skills :: S.Set Skill
  }

data GameState = GameState
  { current_skills :: M.Map Skill Experience,
    available_actions :: S.Set ActionName,
    done_actions :: S.Set ActionName,
    life :: Life,
    max_life :: Life,
    init_life :: Life,
    time :: Time
  }
  deriving (Show)

initialState :: GameDefinition -> GameState
initialState def =
  let initial_actions = computeAvailableActions def state
      state =
        GameState
          { current_skills = M.fromSet (\_ -> Experience 0 0 0 0) (initial_skills def),
            available_actions = initial_actions,
            done_actions = S.empty,
            life = 10,
            max_life = 10,
            init_life = 10,
            time = 0
          }
   in state

resetGame :: GameDefinition -> GameState -> GameState
resetGame def state =
  let new_available_actions = computeAvailableActions def newState
      newState =
        state
          { current_skills = resetSkills $ current_skills state,
            available_actions = new_available_actions,
            done_actions = S.empty,
            life = init_life state,
            max_life = init_life state,
            time = 0
          }
   in newState

setActionDone :: ActionName -> GameState -> GameState
setActionDone action state =
  state
    { available_actions = S.delete action $ available_actions state,
      done_actions = S.insert action $ done_actions state
    }

updateActionList :: GameDefinition -> GameState -> GameState
updateActionList def state = state {available_actions = (available_actions state `S.union` computeAvailableActions def state) `S.difference` done_actions state}

addSkillsExp :: S.Set Skill -> Time -> GameState -> GameState
addSkillsExp skills (Time timePassed) state =
  state {current_skills = M.mapWithKey updateSkill (current_skills state)}
  where
    updateSkill skill exp
      | skill `S.member` skills =
        let speed = toSpeed exp
            newExp =
              exp
                { hardWork = hardWork exp + speed * timePassed,
                  talent = talent exp + speed * timePassed,
                  hardWorkLevel = floor . log $ hardWork newExp,
                  talentLevel = floor . log $ talent newExp
                }
         in newExp
    updateSkill skill exp = exp

passTimeDuringAction :: Action -> GameState -> (GameState, Alive)
passTimeDuringAction action state =
  let skillsForAction = skillsUsed action
      allExperiences = current_skills state
      experienceForSkills = (\s -> maybe 1 toSpeed (s `M.lookup` allExperiences)) <$> S.toList skillsForAction
      multiplication = product experienceForSkills
      timeNeededForAction = Time (cost action / multiplication)
      (lifeTakenDuringAction, maybeTimeBeforeDeath) = computeLife timeNeededForAction (time state) (life state)
   in case maybeTimeBeforeDeath of
        Just timeBeforeDeath -> (addSkillsExp skillsForAction timeBeforeDeath state, False)
        Nothing ->
          let newState =
                state
                  { life = life state - lifeTakenDuringAction,
                    time = time state + timeNeededForAction
                  }
           in (addSkillsExp skillsForAction timeNeededForAction newState, True)

resetSkills :: M.Map Skill Experience -> M.Map Skill Experience
resetSkills = fmap resetExperience

executeAction :: GameDefinition -> GameState -> ActionName -> Maybe GameState
executeAction def state action =
  if action `elem` available_actions state
    then do
      current_action <- M.lookup action (actions def)
      let (new_state, alive) = passTimeDuringAction current_action state
      pure $
        if alive
          then updateActionList def . setActionDone action $ new_state
          else resetGame def new_state
    else Nothing

computeAvailableActions :: GameDefinition -> GameState -> S.Set ActionName
computeAvailableActions def state =
  let availableSkills = M.keysSet $ current_skills state

      filterActions action | not (skillsUsed action `S.isSubsetOf` availableSkills) = False
      filterActions action = case condition action of
        NoCondition -> True
        ActionCondition action_condition -> S.member action_condition (done_actions state)
   in M.keysSet $ M.filter filterActions (actions def)
