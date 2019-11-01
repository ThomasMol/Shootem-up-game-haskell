module View where

import Graphics.Gloss

import Model

view :: GameState -> IO Picture
view = return . viewPure

viewPure :: GameState -> Picture
viewPure (GameState player enemies bullets score) = pictures help
                                                where 
                                                    help = [scale 0.4 0.4 (translate (-300) 10 (color white (text (show score)))),translate (positionX player) (positionY player) (color white (shape player))] ++ test
                                                    test = drawEnemies (GameState player enemies bullets score)
drawEnemies :: GameState -> [Picture]
drawEnemies gstate = map (drawEnemy 1) (enemies gstate)

drawEnemy :: Int -> Enemy -> Picture
drawEnemy  _ enemy = translate(enemyPosX enemy)(enemyPosY enemy)(color red (enemyShape enemy))