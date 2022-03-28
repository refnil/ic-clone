{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE RankNTypes #-}

module Game.State where

import Control.Monad
import Control.Monad.Reader
import Control.Monad.State
import Data.Aeson
import qualified Data.List as L
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Deriving.Aeson.Stock
import GHC.Generics
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
  { current_skills :: M.Map Skill Experience,
    available_actions :: S.Set ActionName,
    done_actions :: S.Set ActionName,
    life :: Life,
    max_life :: Life,
    init_life :: Life,
    elapsedTime :: Time,
    accumulatedTime :: Time
  }
  deriving (Show, Generic)
  deriving (FromJSON, ToJSON) via Snake GameState

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
            elapsedTime = 0,
            accumulatedTime = 0
          }
   in state

type GameMonadT m = StateT GameState (ReaderT GameDefinition m)

type GameMonad a = forall m. Monad m => GameMonadT m a

isAccumulatedTimePositive :: GameMonad Bool
isAccumulatedTimePositive = gets $ (>= 0) . accumulatedTime

resetGame :: GameMonad ()
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

setActionDone :: ActionName -> GameMonad ()
setActionDone action =
  modify
    ( \state ->
        state
          { available_actions = S.delete action $ available_actions state,
            done_actions = S.insert action $ done_actions state
          }
    )

updateActionList :: GameMonad ()
updateActionList = do
  state <- get
  def <- ask
  let allFilteredActions = computeAvailableActions def state
  modify
    ( \state ->
        state
          { available_actions =
              allFilteredActions `S.difference` done_actions state
          }
    )

talentLog, hardWorkLog :: Float -> Int
talentLog = floor . logBase 1.5
hardWorkLog = floor . logBase 1.5

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
                  hardWorkLevel = hardWorkLog $ hardWork newExp,
                  talentLevel = talentLog $ talent newExp
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
executeAction name = do
  state <- get
  action <- getAction name
  case action of
    Nothing -> return $ Just ActionUnknown
    Just wantedAction -> do
      if name `elem` available_actions state
        then do
          alive <- passTimeDuringAction wantedAction
          if alive
            then do
              setActionDone name
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

computeAvailableActions :: GameDefinition -> GameState -> S.Set ActionName
computeAvailableActions def state = M.keysSet $ M.filter (checkActionCondition state) (actions def)

getAction :: ActionName -> GameMonad (Maybe Action)
getAction name = asks (M.lookup name . actions)
