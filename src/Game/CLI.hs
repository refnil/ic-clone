{-# LANGUAGE OverloadedStrings #-}
module Game.CLI where

import Data.Text as T
import Text.Read
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Prelude as P
import Data.List as L
import Data.Maybe
import Control.Monad

import Game.State
import Game.Action
import Game.Base

menu :: [(Text, IO ())] -> IO ()
menu choices = do
  result_map <- forM (P.zip [1 ..] choices) $ \(i, (message, choice)) -> do
    putStrLn $ show i <> ": " <> unpack message
    return (i, choice)
  readValue <- readMaybe <$> getLine 
  case readValue of
    Nothing -> menu choices
    Just index -> case L.lookup index result_map of
                    Nothing -> menu choices
                    Just todo -> todo

prepareAction :: GameDefinition -> GameState -> ActionName -> (Text, IO ())
prepareAction def state a = (
        let (Just action) = M.lookup a (actions def)
            messageText = message action
            skillsText = "(" <> T.unwords (fmap (pack . show) $ S.toList $ skillsUsed action) <> ")"
            costText = pack . show . cost $ action
         in T.unwords [skillsText, messageText, costText],
        do let (Just newState) = executeAction def state a
           runGame def newState
    )

chooseAction :: GameDefinition -> GameState -> IO ()
chooseAction def state = do
  putStrLn "Choose an action:"
  let actions_for_menu = prepareAction def state <$> S.toList (available_actions state)
      print_state = ("Print current game state", print state >> runGame def state) 
      die = ("Die", runGame def (resetGame def state))
      quit = ("Quit", return ())
  menu (actions_for_menu ++ [print_state, die, quit])

runGame :: GameDefinition -> GameState -> IO ()
runGame def state = do
    putStrLn $ show (life state) <> " " <> show (time state)
    putStrLn $ M.foldMapWithKey (\skill exp -> show skill <> "(" <> show (toSpeed exp) <> ") ") $ current_skills state
    chooseAction def state


runNewGame :: GameDefinition -> IO ()
runNewGame def = runGame def (initialState def)
