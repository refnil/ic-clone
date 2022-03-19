{-# LANGUAGE OverloadedStrings #-}
module Game.CLI where

import Data.Text
import Text.Read
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Prelude as P
import Data.List as L
import Data.Maybe
import Control.Monad

import Game.State
import Game.Action

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
