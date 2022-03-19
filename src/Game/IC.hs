module Game.IC where

import Game.CLI
import Game.Definition
import Game.State

main :: IO ()
main = runNewGame fullGame
