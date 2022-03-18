{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
module Game.IC where

import Control.Monad
import qualified Data.List as L
import qualified Data.Map.Strict as M
import Data.Maybe
import qualified Data.Set as S
import Data.Text
import GHC.Float
import Text.Read
import Prelude as P

data Skill = Sword |
             Bow |
             Axe |
             Lance |
             Fight |
             Kata |
             Walking |
             Talking |
             Manipulation
  deriving (Eq, Ord, Show)

data Experience = Experience
  { hardWork :: Float,
    talent :: Float,
    hardWorkLevel :: Int,
    talentLevel :: Int
  }
  deriving (Show)

data ActionName = ActionName Text Int
  deriving (Eq, Ord, Show)

data Action = Action
  { skillsUsed :: S.Set Skill,
    message :: Text,
    condition :: Condition,
    award :: Award,
    cost :: Float
  }

data Condition
  = NoCondition
  | ActionCondition ActionName

newtype Award = Award {runAward :: GameState -> GameState}

newtype Time = Time Float
  deriving (Show)
  deriving newtype (Eq, Ord, Num)

newtype Life = Life Float
  deriving (Show)
  deriving newtype (Eq, Ord, Num)

type Alive = Bool

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

setActionDone :: ActionName -> GameState -> GameState
setActionDone action state =
  state
    { available_actions = S.delete action $ available_actions state,
      done_actions = S.insert action $ done_actions state
    }

updateActionList :: GameDefinition -> GameState -> GameState
updateActionList def state = state {available_actions = (available_actions state `S.union` computeAvailableActions def state) `S.difference` done_actions state}

toSpeed :: Experience -> Float
toSpeed exp = 1.05 ** int2Float (hardWorkLevel exp) * 1.01 ** int2Float (talentLevel exp)

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

resetExperience :: Experience -> Experience
resetExperience exp = exp {hardWork = 0, hardWorkLevel = 1}

resetSkills :: M.Map Skill Experience -> M.Map Skill Experience
resetSkills = fmap resetExperience

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

executeAction :: GameDefinition -> GameState -> ActionName -> Maybe GameState
executeAction def state action =
  if action `elem` available_actions state
    then do
      current_action <- M.lookup action (actions def)
      let (new_state, alive) = passTimeDuringAction current_action state
      pure $
        if alive
          then
            let current_award = runAward . award $ current_action
             in updateActionList def . current_award . setActionDone action $ new_state
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

simpleGame :: GameDefinition
simpleGame =
  GameDefinition
    { actions =
        M.fromList
          [ (ActionName "Wake up" 0, Action (S.singleton Manipulation) "Open your eyes." NoCondition (Award id) 10),
            (ActionName "Open door" 0, Action (S.singleton Manipulation) "Turn the handle." (ActionCondition (ActionName "Wake up" 0)) (Award id) 10)
          ],
      initial_skills = S.fromList [ Walking, Talking, Manipulation ]
    }

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

menu :: [(Text, IO ())] -> IO ()
menu choices = do
  result_map <- forM (P.zip [1 ..] choices) $ \(i, (message, choice)) -> do
    putStrLn $ show i <> ": " <> unpack message
    return (i, choice)
  readValue <- readMaybe <$> getLine 
  case readValue of
    Nothing -> menu choices
    Just index -> case (L.lookup index result_map) of
                    Nothing -> menu choices
                    Just todo -> todo

prepareAction :: GameDefinition -> GameState -> ActionName -> (Text, IO ())
prepareAction def state a = (
        fromJust $ message <$> M.lookup a (actions def),
        do let (Just newState) = executeAction def state a
           runGame def newState
    )

chooseAction :: GameDefinition -> GameState -> IO ()
chooseAction def state = do
  putStrLn "Choose an action"
  let actions_for_menu = fmap (prepareAction def state) $ S.toList (available_actions state)
      print_state = ("Print current game state", print state >> runGame def state) 
      quit = ("Quit", return ())
  menu (actions_for_menu ++ [print_state, quit])

runGame :: GameDefinition -> GameState -> IO ()
runGame def state = chooseAction def state

main :: IO ()
main = runGame simpleGame (initialState simpleGame)