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

menu :: MonadIO m => [(Text, m ())] -> m () -> m ()
menu choices whenInvalid = do
  result_map <- forM (P.zip [1 ..] choices) $ \(i, (message, choice)) -> do
    liftIO $ putStrLn $ show i <> ": " <> unpack message
    return (i, choice)
  readValue <- R.readMaybe <$> liftIO getLine
  case readValue of
    Nothing -> whenInvalid
    Just index -> fromMaybe whenInvalid (L.lookup index result_map)

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
        runGame
    )

chooseAction :: GameMonadIO ()
chooseAction = do
  state <- get
  liftIO $ putStrLn "Choose an action:"
  canDoAction <- isAccumulatedTimePositive
  actions_for_menu <- if canDoAction
     then sequence $ prepareAction <$> S.toList (available_actions state)
     else return []
  let wait = ("Wait some time ", runGame)
      print_state = ("Print current game state", liftIO (print state) >> runGame)
      die = ("Die", resetGame >> runGame)
      quit = ("Quit", return ())
  menu (actions_for_menu ++ [wait, print_state, die, quit]) runGame

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
