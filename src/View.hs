module View where

import Graphics.Gloss
import Model

view :: GameState -> IO Picture
view = return . viewPure