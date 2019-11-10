module Main where

import Controller
import Model
import View
    
import Graphics.Gloss.Interface.IO.Game

main = playIO (InWindow "Shoot’em Up" (800, 600) (0, 0)) -- Or FullScreen
              black            -- Background color
              10               -- Frames per second
              initialState     -- Initial state
              view             -- View function
              input            -- Event function
              step             -- Step function