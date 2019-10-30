module View where

import Graphics.Gloss

import Model

view :: GameState -> IO Picture
view = return . viewPure

viewPure :: GameState -> Picture
viewPure (GameState player score) = pictures [scale 0.4 0.4 (translate (-300) 10 (color white (text (show score)))),color white (shape player)]