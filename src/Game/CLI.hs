{-# LANGUAGE OverloadedStrings #-}

module Game.CLI where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.State
import Data.List as L
import qualified Data.Map.Strict as M
import Data.Maybe
import qualified Data.Set as S
import Data.Text as T
import Game.Action
import Game.Base
import Game.State
import qualified Text.Read as R
import Prelude as P
import qualified Data.Time.Clock as C

data CLIState = CLIState { lastTime :: Maybe C.UTCTime }

type GameMonadIO = GameMonadT (StateT CLIState IO)

getTimeSinceLastTime :: GameMonadIO Time
getTimeSinceLastTime = do
    currentTime <- liftIO $ C.getCurrentTime
    lift $ state (\s -> (Time $ maybe 0 (C.diffUTCTime currentTime) (lastTime s), s { lastTime = Just currentTime }))

menu :: MonadIO m => [(String, Text, m a)] -> m a -> m a 
menu choices whenInvalid = do
  result_map <- forM choices $ \(key, message, choice) -> do
    liftIO $ putStrLn $ key <> ": " <> unpack message
    return (key, choice)
  readValue <- liftIO getLine
  fromMaybe whenInvalid (L.lookup readValue result_map)

prepareAction :: ActionName -> GameMonadIO (Text, GameMonadIO ())
prepareAction a = do
  state <- get
  let def = gameDefinition state
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

chooseAction :: GameMonadIO ()
chooseAction = do
  state <- get
  canDoAction <- isAccumulatedTimePositive
  actionsToDo <- sequence $ prepareAction <$> S.toList (available_actions state)
  let actionsForMenu = P.zipWith (\i (t,a) -> (show i, t, a >> return True)) [1..] actionsToDo

      wait = ("w", "Wait some time ", return True)
      print_state = ("p", "Print current game state", liftIO (print state) >> return True)
      die = ("d", "Die", resetGame >> return True)
      quit = ("q", "Quit", return False)
      menuChoices = (if canDoAction then actionsForMenu else []) ++ [wait, print_state, die, quit]
  unless canDoAction $ liftIO $ do
      putStrLn $ "Available actions in " <> show (negate $ accumulatedTime state)
      forM_ actionsForMenu $ \(k, t, _) ->
          putStrLn $ k <> ": " <> unpack t
  liftIO $ putStrLn "Choose an action:"
  continue <- menu menuChoices (return True)
  when continue runGame

runGame :: GameMonadIO ()
runGame = do
  diffTime <- getTimeSinceLastTime
  modify (\s -> s { accumulatedTime = accumulatedTime s + diffTime })
  state <- get
  liftIO . putStrLn $ show (life state) <> " " <> show (elapsedTime state)
  liftIO . putStrLn $ M.foldMapWithKey (\skill exp -> show skill <> "(" <> show (toSpeed exp) <> ") ") $ current_skills state
  chooseAction

initialCLIState :: CLIState
initialCLIState = CLIState Nothing

runNewGame :: GameDefinition -> IO ()
runNewGame def = evalStateT (evalStateT runGame (initialState def)) initialCLIState
