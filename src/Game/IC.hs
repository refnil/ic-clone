module Game.IC where

import Game.CLI
import Game.Definition
import Game.State

main :: IO ()
main = do
    saveData <- loadSave
    maybe runNewGame runFromSave saveData fullGame
