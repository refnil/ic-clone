{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Game.CLI where

import Control.Concurrent
import Control.Concurrent.Async as A
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Monad.State
import Data.Aeson
import Data.Function
import Data.List as L
import qualified Data.Map.Strict as M
import Data.Maybe
import qualified Data.Set as S
import Data.Text as T
import qualified Data.Time.Clock as C
import Deriving.Aeson.Stock
import Game.Action
import Game.Base
import Game.State
import qualified Text.Read as R
import qualified Text.Read as Read
import Prelude as P

data AutoplayMode = SmallestCost | LargestCost
  deriving (Show, Enum, Bounded, Generic)
  deriving (FromJSON, ToJSON) via Snake AutoplayMode

data CLIState = CLIState
  { lastTime :: Maybe C.UTCTime,
    autoplayActivated :: Bool,
    autoplayMode :: AutoplayMode,
    debugAutoplay :: Bool
  }
  deriving (Generic)
  deriving (FromJSON, ToJSON) via Snake CLIState

type GameMonadIO = GameMonadT (StateT CLIState IO)

saveFilename :: String
saveFilename = ".ic-clone.save.json"

getTimeSinceLastTime :: GameMonadIO Time
getTimeSinceLastTime = do
  currentTime <- liftIO $ C.getCurrentTime
  lift $ state (\s -> (Time $ maybe 0 (C.diffUTCTime currentTime) (lastTime s), s {lastTime = Just currentTime}))

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

mkDebugAutoplayMenu :: GameMonadIO (String, Text, GameMonadIO Bool)
mkDebugAutoplayMenu = do
  activated <- lift $ gets debugAutoplay
  let key = "f"
      text =
        if activated
          then "Stop fast autoplay"
          else "Enable fast autoplay"
      toggle = lift $ modify (\s -> s {debugAutoplay = not $ debugAutoplay s}) >> return True
  return (key, text, toggle)

mkToggleAutoplayMenu :: GameMonadIO (String, Text, GameMonadIO Bool)
mkToggleAutoplayMenu = do
  activated <- lift $ gets autoplayActivated
  let key = "t"
      text =
        if activated
          then "Disactivate autoplay"
          else "Activate autoplay"
      toggle = lift $ modify (\s -> s {autoplayActivated = not $ autoplayActivated s}) >> return True
  return (key, text, toggle)

mkAutoplayModeMenu :: GameMonadIO (String, Text, GameMonadIO Bool)
mkAutoplayModeMenu = do
  mode <- lift $ gets autoplayMode
  return
    ( "a",
      "Change autoplay mode (current: " <> pack (show mode) <> ")",
      changeAutoMode >> return True
    )

changeAutoMode :: GameMonadIO ()
changeAutoMode = do
  liftIO $ do
    putStrLn ""
    putStrLn "Autoplay mode selection menu"
    putStrLn "==========================="
  join . liftIO $ menu (P.zipWith makeMenu [1 ..] [minBound .. maxBound]) (return ())
  where
    makeMenu :: Int -> AutoplayMode -> (String, Text, GameMonadIO ())
    makeMenu index mode = (show index, pack (show mode), lift $ modify (\s -> s {autoplayMode = mode}))

addTimeDebug :: GameMonadIO Bool
addTimeDebug = do
  newTime <- liftIO $ do
    putStrLn "How much seconds to add?"
    Read.readMaybe <$> getLine
  maybe (return ()) (addTime . seconds) newTime
  return True

debugMenu :: (String, Text, GameMonadIO Bool)
debugMenu = ("x", "Debug menu",) $ do
    debugAutoplay <- mkDebugAutoplayMenu
    liftIO $ do putStrLn ""
                putStrLn "Debug menu"
                putStrLn "=========="
    join . liftIO $ menu [debugAutoplay] (return True)

chooseAction :: GameMonadIO ()
chooseAction = do
  state <- get
  canDoAction <- isAccumulatedTimePositive
  actionsToDo <- sequence $ prepareAction <$> S.toList (available_actions state)

  toggleAutoplay <- mkToggleAutoplayMenu
  autoplayModeMenu <- mkAutoplayModeMenu
  let actionsForMenu = P.zipWith (\i (t, a) -> (show i, t, a >> return True)) [1 ..] actionsToDo

      wait = ("w", "Wait some time ", return True)
      print_state = ("p", "Print current game state", liftIO (print state) >> return True)
      die = ("d", "Die", resetGame >> return True)
      quit = ("q", "Quit", return False)
      addTime = ("y", "Add time", addTimeDebug)

      menuChoices = (if canDoAction then actionsForMenu else []) ++ [wait, toggleAutoplay, autoplayModeMenu, addTime, debugMenu, die, print_state, quit]
  unless canDoAction $
    liftIO $ do
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
        else Just . fst $ minimumBy (compare `on` (cost . snd)) (M.toList currentActions)
findAutoplayAction LargestCost def state =
  let allActions = actions def
      currentActions = allActions `M.restrictKeys` (available_actions state)
   in if M.null currentActions
        then Nothing
        else Just . fst $ maximumBy (compare `on` (cost . snd)) (M.toList currentActions)

chooseAutoplayAction :: GameMonadIO (Maybe ActionName)
chooseAutoplayAction = do
  activated <- lift $ gets autoplayActivated
  mode <- lift $ gets autoplayMode
  state <- get
  def <- ask
  pure $ guard activated >> findAutoplayAction mode def state

prepareAutoplay :: GameMonadIO (IO (GameMonadIO ()))
prepareAutoplay = do
  maybeName <- chooseAutoplayAction
  timeToWaitForAccTime <- gets (negate . toSeconds . accumulatedTime)
  isDebug <- lift $ gets debugAutoplay

  return (asyncAction maybeName timeToWaitForAccTime (if isDebug then 1000 else 1))
      where asyncAction :: Maybe ActionName -> Int -> Int -> IO (GameMonadIO ())
            asyncAction Nothing _ repeat = do
              threadDelay maxBound
              asyncAction Nothing 0 repeat
            asyncAction (Just name) wait repeat = do
              threadDelay $ max 1000000 (wait * 1000000)
              return (runAutoplay name repeat)
                where 
                  runAutoplay _ 0 = return ()
                  runAutoplay name repeat = do
                    maybeAction <- getAction name
                    liftIO $ putStrLn $ "Autoplay: " <> maybe "Unknown action" (unpack . message) maybeAction
                    res <- executeAction name
                    case res of
                      (Just _) -> return ()
                      Nothing -> do
                        maybeName <- chooseAutoplayAction
                        case maybeName of
                          (Just nextName) -> runAutoplay nextName (repeat - 1)
                          Nothing -> return ()

addTime :: Time -> GameMonadIO ()
addTime time = modify (\s -> s {accumulatedTime = accumulatedTime s + time})

runGame :: GameMonadIO ()
runGame = do
  diffTime <- getTimeSinceLastTime
  addTime diffTime
  saveGame
  state <- get
  liftIO . putStrLn $ show (life state) <> " " <> show (elapsedTime state)
  liftIO . putStrLn $ M.foldMapWithKey (\skill exp -> show skill <> "(" <> show (toSpeed exp) <> ") ") $ current_skills state
  chooseAction

initialCLIState :: CLIState
initialCLIState = CLIState Nothing False SmallestCost False

data SaveData = SaveData
  { saveState :: GameState,
    saveOption :: CLIState
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
