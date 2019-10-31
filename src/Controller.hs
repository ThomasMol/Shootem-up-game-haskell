module Controller where

import Model
    
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Random

-- | Handle one iteration of the game
step :: Float -> GameState -> IO GameState
step secs gstate = return $ gstate 

input :: Event -> GameState -> IO GameState
input e gstate = return (inputKey e gstate)

inputKey :: Event -> GameState -> GameState
inputKey (EventKey (SpecialKey c) Down _ _) (GameState player score ) | c == KeyUp = GameState player {positionY = positionY player + 10} score
                                                                      | c == KeyDown = GameState player {positionY = positionY player - 10} score

inputKey _ gstate = gstate -- Otherwise keep the same