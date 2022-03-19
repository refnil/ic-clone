module Game.State where

import Control.Monad
import Control.Monad.State
import qualified Data.List as L
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Game.Action
import Game.Base
import Game.Skill

data GameDefinition = GameDefinition
  { actions :: M.Map ActionName Action,
    initial_skills :: S.Set Skill
  }
  deriving (Show)

instance Semigroup GameDefinition where
  (<>) def1 def2 =
    GameDefinition
      { actions = actions def1 <> actions def2,
        initial_skills = initial_skills def1 <> initial_skills def2
      }

instance Monoid GameDefinition where
  mempty = GameDefinition mempty mempty

data GameState = GameState
  { gameDefinition :: GameDefinition,
    current_skills :: M.Map Skill Experience,
    available_actions :: S.Set ActionName,
    done_actions :: S.Set ActionName,
    life :: Life,
    max_life :: Life,
    init_life :: Life,
    elapsedTime :: Time,
    accumulatedTime :: Time
  }
  deriving (Show)

initialState :: GameDefinition -> GameState
initialState def =
  let initial_actions = computeAvailableActions state
      state =
        GameState
          { gameDefinition = def,
            current_skills = M.fromSet (\_ -> Experience 0 0 0 0) (initial_skills def),
            available_actions = initial_actions,
            done_actions = S.empty,
            life = 10,
            max_life = 10,
            init_life = 10,
            elapsedTime = 0,
            accumulatedTime = 0
          }
   in state

resetGame :: Monad m => GameMonadT m ()
resetGame = do
  modify
    ( \state ->
        state
          { current_skills = resetSkills $ current_skills state,
            available_actions = mempty,
            done_actions = S.empty,
            life = init_life state,
            max_life = init_life state,
            elapsedTime = 0
          }
    )
  updateActionList

type GameMonadT = StateT GameState

setActionDone :: Monad m => ActionName -> GameMonadT m ()
setActionDone action =
  modify
    ( \state ->
        state
          { available_actions = S.delete action $ available_actions state,
            done_actions = S.insert action $ done_actions state
          }
    )

updateActionList :: Monad m => GameMonadT m ()
updateActionList = do
  state <- get
  let allFilteredActions = computeAvailableActions state
  modify
    ( \state ->
        state
          { available_actions =
              allFilteredActions `S.difference` done_actions state
          }
    )

addSkillsExp :: Monad m => S.Set Skill -> Time -> GameMonadT m ()
addSkillsExp skills timePassed =
  modify
    ( \state ->
        state {current_skills = M.mapWithKey updateSkill (current_skills state)}
    )
  where
    timePassedF = toSecondsF timePassed
    updateSkill skill exp
      | skill `S.member` skills =
        let speed = toSpeed exp
            newExp =
              exp
                { hardWork = hardWork exp + speed * timePassedF,
                  talent = talent exp + speed * timePassedF,
                  hardWorkLevel = floor . log $ hardWork newExp,
                  talentLevel = floor . log $ talent newExp
                }
         in newExp
    updateSkill skill exp = exp

passTimeDuringAction :: Monad m => Action -> GameMonadT m Alive
passTimeDuringAction action = do
  state <- get
  let skillsForAction = skillsUsed action
      allExperiences = current_skills state
      experienceForSkills = (\s -> maybe 1 toSpeed (s `M.lookup` allExperiences)) <$> S.toList skillsForAction
      multiplication = product experienceForSkills
      timeNeededForAction = secondsF (cost action / multiplication)
      (lifeTakenDuringAction, maybeTimeBeforeDeath) = computeLife timeNeededForAction (elapsedTime state) (life state)
  case maybeTimeBeforeDeath of
    Just timeBeforeDeath -> do
      addSkillsExp skillsForAction timeBeforeDeath
      return False
    Nothing -> do
      modify
        ( \state ->
            state
              { life = life state - lifeTakenDuringAction,
                elapsedTime = elapsedTime state + timeNeededForAction,
                accumulatedTime = accumulatedTime state - timeNeededForAction
              }
        )
      addSkillsExp skillsForAction timeNeededForAction
      return True

resetSkills :: M.Map Skill Experience -> M.Map Skill Experience
resetSkills = fmap resetExperience

data ExecuteActionError = ActionNotAvailable | ActionUnknown
  deriving (Show)

executeAction :: Monad m => ActionName -> GameMonadT m (Maybe ExecuteActionError)
executeAction action = do
  state <- get
  let def = gameDefinition state
  case M.lookup action (actions def) of
    Nothing -> return $ Just ActionUnknown
    Just wantedAction -> do
      if action `elem` available_actions state
        then do
          alive <- passTimeDuringAction wantedAction
          if alive
            then do
              setActionDone action
              updateActionList
              return Nothing
            else do
              resetGame
              return Nothing
        else return $ Just ActionNotAvailable

checkActionCondition :: GameState -> Action -> Bool
checkActionCondition state action | not (skillsUsed action `S.isSubsetOf` M.keysSet (current_skills state)) = False
checkActionCondition state action = checkCondition state (condition action)

checkCondition :: GameState -> Condition -> Bool
checkCondition state NoCondition = True
checkCondition state (ActionCondition name) = S.member name (done_actions state)
checkCondition state (OrCondition conds) = any (checkCondition state) conds
checkCondition state (AndCondition conds) = all (checkCondition state) conds
checkCondition state (NotCondition cond) = not (checkCondition state cond)

computeAvailableActions :: GameState -> S.Set ActionName
computeAvailableActions state = M.keysSet $ M.filter (checkActionCondition state) (actions . gameDefinition $ state)
