{-# LANGUAGE OverloadedStrings #-}
module Game.CLI where

import Data.Text as T
import qualified Text.Read as R
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Prelude as P
import Data.List as L
import Data.Maybe
import Control.Monad
import Control.Monad.State
import Control.Monad.IO.Class

import Game.State
import Game.Action
import Game.Base

type GameMonadIO = GameMonadT IO

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
    return (
        let (Just action) = M.lookup a (actions def)
            messageText = message action
            skillsText = "(" <> T.unwords (fmap (pack . show) $ S.toList $ skillsUsed action) <> ")"
            costText = pack . show . cost $ action
         in T.unwords [skillsText, messageText, costText],
        do actionError <- executeAction a
           case actionError of
             Nothing -> return ()
             Just error -> liftIO $ print error
           runGame
     )

chooseAction :: GameMonadIO ()
chooseAction = do
  state <- get
  liftIO $ putStrLn "Choose an action:"
  actions_for_menu <- sequence $ prepareAction <$> S.toList (available_actions state)
  let print_state = ("Print current game state", liftIO (print state) >> runGame) 
      die = ("Die", resetGame >> runGame)
      quit = ("Quit", return ())
  menu (actions_for_menu ++ [print_state, die, quit]) runGame

runGame :: GameMonadIO ()
runGame = do
    state <- get
    liftIO . putStrLn $ show (life state) <> " " <> show (elapsedTime state)
    liftIO . putStrLn $ M.foldMapWithKey (\skill exp -> show skill <> "(" <> show (toSpeed exp) <> ") ") $ current_skills state
    chooseAction

runNewGame :: GameDefinition -> IO ()
runNewGame def = evalStateT runGame (initialState def)
