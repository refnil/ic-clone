module Game.IC where

import Game.CLI
import Game.State
import Game.Definition

main :: IO ()
main = runNewGame fullGame
