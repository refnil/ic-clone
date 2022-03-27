{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}

module Game.CLI where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.State
import Control.Monad.Reader
import Data.List as L
import qualified Data.Map.Strict as M
import Data.Maybe
import qualified Data.Set as S
import Data.Text as T
import Data.Function
import Data.Aeson
import Deriving.Aeson.Stock
import Game.Action
import Game.Base
import Game.State
import qualified Text.Read as R
import Prelude as P
import qualified Data.Time.Clock as C
import Control.Concurrent.Async as A
import Control.Concurrent

data AutoplayMode = SmallestCost | LargestCost
    deriving (Show, Enum, Bounded, Generic)
    deriving (FromJSON, ToJSON) via Snake AutoplayMode

data CLIState = CLIState { lastTime :: Maybe C.UTCTime,
                           autoplayActivated :: Bool,
                           autoplayMode :: AutoplayMode
                         }
    deriving (Generic)
    deriving (FromJSON, ToJSON) via Snake CLIState

type GameMonadIO = GameMonadT (StateT CLIState IO)

saveFilename :: String
saveFilename = ".ic-clone.save.json"

getTimeSinceLastTime :: GameMonadIO Time
getTimeSinceLastTime = do
    currentTime <- liftIO $ C.getCurrentTime
    lift $ state (\s -> (Time $ maybe 0 (C.diffUTCTime currentTime) (lastTime s), s { lastTime = Just currentTime }))

menu :: [(String, Text, a)] -> a -> IO a
menu choices whenInvalid = do
  result_map <- forM choices $ \(key, message, choice) -> do
    putStrLn $ key <> ": " <> unpack message
    return (key, choice)
  readValue <- getLine
  return $ fromMaybe whenInvalid (L.lookup readValue result_map)

prepareAction :: ActionName -> GameMonadIO (Text, GameMonadIO ())
prepareAction a = do
  state <- get
  def <- ask
  return
    ( let (Just action) = M.lookup a (actions def)
          messageText = message action
          skillsText = "(" <> T.unwords (fmap (pack . show) $ S.toList $ skillsUsed action) <> ")"
          costText = pack . show . cost $ action
       in T.unwords [skillsText, messageText, costText],
      do
        actionError <- executeAction a
        case actionError of
          Nothing -> return ()
          Just error -> liftIO $ print error
    )

mkToggleAutoplayMenu :: GameMonadIO (String, Text, GameMonadIO Bool)
mkToggleAutoplayMenu = do
    activated <- lift $ gets autoplayActivated
    let key = "t"
        text = if activated 
                  then "Disactivate autoplay"
                  else "Activate autoplay"
        toggle = lift $ modify (\s -> s { autoplayActivated = not $ autoplayActivated s }) >> return True
    return (key, text, toggle)

mkAutoplayModeMenu :: GameMonadIO (String, Text, GameMonadIO Bool)
mkAutoplayModeMenu = do
    mode <- lift $ gets autoplayMode
    return (
            "a",
            "Change autoplay mode (current: " <> pack (show mode) <> ")",
            changeAutoMode >> return True
        )
        

changeAutoMode :: GameMonadIO ()
changeAutoMode = do
    liftIO $ do
        putStrLn ""
        putStrLn "Autoplay mode selection menu"
        putStrLn "==========================="
    join . liftIO $ menu (P.zipWith makeMenu [1..] [minBound..maxBound]) (return ())
    where 
        makeMenu :: Int -> AutoplayMode -> (String, Text, GameMonadIO ()) 
        makeMenu index mode = (show index, pack (show mode), lift $ modify (\s -> s { autoplayMode = mode }))

chooseAction :: GameMonadIO ()
chooseAction = do
  state <- get
  canDoAction <- isAccumulatedTimePositive
  actionsToDo <- sequence $ prepareAction <$> S.toList (available_actions state)

  toggleAutoplay <- mkToggleAutoplayMenu
  autoplayModeMenu <- mkAutoplayModeMenu
  let actionsForMenu = P.zipWith (\i (t,a) -> (show i, t, a >> return True)) [1..] actionsToDo

      wait = ("w", "Wait some time ", return True)
      print_state = ("p", "Print current game state", liftIO (print state) >> return True)
      die = ("d", "Die", resetGame >> return True)
      quit = ("q", "Quit", return False)

      menuChoices = (if canDoAction then actionsForMenu else []) ++ [wait, toggleAutoplay, autoplayModeMenu, die, print_state, quit]
  unless canDoAction $ liftIO $ do
      putStrLn $ "Available actions in " <> show (negate $ accumulatedTime state)
      forM_ actionsForMenu $ \(k, t, _) ->
          putStrLn $ k <> ": " <> unpack t
  liftIO $ putStrLn "Choose an action:"
  waitAutoplay <- prepareAutoplay
  choiceOrAutoplay <- liftIO $ race (menu menuChoices (return True)) waitAutoplay
  let nextStep = liftIO (putStrLn "") >> runGame
  either (>>= flip when nextStep) (>> nextStep) choiceOrAutoplay

findAutoplayAction :: AutoplayMode -> GameDefinition -> GameState -> Maybe ActionName
findAutoplayAction SmallestCost def state = 
    let allActions = actions def
        currentActions = allActions `M.restrictKeys` (available_actions state)
     in if M.null currentActions 
           then Nothing
           else Just . fst $ minimumBy (compare `on` (cost.snd)) (M.toList currentActions)

findAutoplayAction LargestCost def state = 
    let allActions = actions def
        currentActions = allActions `M.restrictKeys` (available_actions state)
     in if M.null currentActions 
           then Nothing
           else Just . fst $ maximumBy (compare `on` (cost.snd)) (M.toList currentActions)

chooseAutoplayAction :: GameMonadIO (Maybe ActionName)
chooseAutoplayAction = do
    activated <- lift $ gets autoplayActivated
    mode <- lift $ gets autoplayMode
    state <- get
    def <- ask
    pure $ guard activated >> findAutoplayAction mode def state
              

prepareAutoplay :: GameMonadIO (IO (GameMonadIO ()))
prepareAutoplay = do
    maybeActionName <- chooseAutoplayAction

    timeToWaitForAccTime <- gets (negate . toSeconds . accumulatedTime)
    maybeAction <- join <$> sequence (getAction <$> maybeActionName)
    let asyncAction = case maybeAction of
            Nothing -> do
                threadDelay maxBound
                asyncAction
            Just action -> do
                threadDelay $ max 1000000 (timeToWaitForAccTime * 1000000)
                return $ do
                    liftIO $ putStrLn $ "Autoplay: " <> unpack (message action)
                    maybe undefined executeAction maybeActionName
                    return ()
    return asyncAction


runGame :: GameMonadIO ()
runGame = do
  diffTime <- getTimeSinceLastTime
  modify (\s -> s { accumulatedTime = accumulatedTime s + diffTime })
  saveGame
  state <- get
  liftIO . putStrLn $ show (life state) <> " " <> show (elapsedTime state)
  liftIO . putStrLn $ M.foldMapWithKey (\skill exp -> show skill <> "(" <> show (toSpeed exp) <> ") ") $ current_skills state
  chooseAction

initialCLIState :: CLIState
initialCLIState = CLIState Nothing False SmallestCost

data SaveData = SaveData 
                { saveState :: GameState
                , saveOption :: CLIState
                }
    deriving (Generic)
    deriving (ToJSON, FromJSON) via Snake SaveData

saveGame :: GameMonadIO ()
saveGame = do
    state <- get
    option <- lift $ get
    liftIO $ encodeFile saveFilename (SaveData state option)

run :: GameDefinition -> GameState -> CLIState -> IO ()
run def state = evalStateT (runReaderT (evalStateT runGame state) def)

runNewGame :: GameDefinition -> IO ()
runNewGame def = run def (initialState def) initialCLIState

runFromSave :: SaveData -> GameDefinition -> IO ()
runFromSave (SaveData state option) def = do
    putStrLn "Loading save"
    run def state option

loadSave :: IO (Maybe SaveData)
loadSave = decodeFileStrict saveFilename
