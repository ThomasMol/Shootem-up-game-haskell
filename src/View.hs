module View where

import Graphics.Gloss
import Control.Exception
import System.IO

import Model

view :: GameState -> IO Picture
view gstate@(GameState player enemies bullets time score animations status) | status == Highscores = do
                                                                                        contents <- (readFile "highscores.txt") `catch` readHandler
                                                                                        return (pictures (drawHighscores (lines contents)))                                                                                         
                                                                 | otherwise =  return (viewPure gstate)
readHandler:: IOError -> IO String
readHandler e = return "Could not find any highscores..."

viewPure :: GameState -> Picture


--Game in Menu
viewPure (GameState player enemies bullets time score animations Menu {menuItem = x}) = pictures [scale 0.2 0.2 (translate (-1000) 300 (color white (text "New Game"))),
                                                                                        scale 0.2 0.2 (translate (-1000) 150 (color white (text "Highscores"))),
                                                                                        scale 0.2 0.2 (translate (-1000) 0 (color white (text "Quit"))),
                                                                                        drawMenuIndicators x]
--Game is paused
viewPure (GameState player enemies bullets time score animations Paused) = pictures [scale 0.2 0.2 (translate (-1000) 300 (color white (text "Game is paused, press p again to unpause"))),
                                                                            translate (positionX player) (positionY player) (color white (shape player))]
--Game is playing
viewPure (GameState player enemies bullets time score animations Playing) = pictures help
                                                where 
                                                    help = [scale 0.4 0.4 (translate (800) 600 (color white (text (show score)))),translate (positionX player) (positionY player) (color white (shape player))] ++ enemydrawing ++ bulletdrawing ++ [playerdrawing] ++ deadAnimation
                                                    enemydrawing = drawEnemies (GameState player enemies bullets time score animations Playing)
                                                    bulletdrawing = drawBullets (GameState player enemies bullets time score animations Playing)
                                                    playerdrawing = drawPlayer(player )
                                                    deadAnimation = drawAnimation (GameState player enemies bullets time score animations Playing)

drawMenuIndicators :: Float -> Picture
drawMenuIndicators x = scale 0.2 0.2 (translate (-1200) ((x*(-150)) + 300) (color white (text ">" )))

drawHighscores :: [String] -> [Picture]
drawHighscores xs = [scale 0.2 0.2 (translate (-1000) 300 (color white (text "Press space to go back."))),scale 0.2 0.2 (translate (-1000) 150 (color blue (text "Highscores:")))] ++ map f xs
        where f x = scale 0.2 0.2 (translate (-1000) 0 (color white (text x)))                                   


drawBullets :: GameState -> [Picture]
drawBullets gstate = map drawBullet (bullets gstate)

drawBullet :: Bullet -> Picture
drawBullet bullet = translate(bulletPosX bullet)(bulletPosY bullet)(color white (ThickCircle 2 4))

drawEnemies :: GameState -> [Picture]
drawEnemies gstate = map drawEnemy  (enemies gstate)

drawEnemy :: Enemy -> Picture
drawEnemy enemy = translate(enemyPosX enemy)(enemyPosY enemy)(color red (ThickCircle 10 20 ))


drawAnimation :: GameState -> [Picture]
drawAnimation gstate = map drawDeadEnemy (animations gstate)

drawDeadEnemy :: Enemy -> Picture
drawDeadEnemy enemy |enemyValue enemy == (-3) = translate(enemyPosX enemy)(enemyPosY enemy)(color yellow (ThickCircle 10 1 )) 
                    |enemyValue enemy == (-2) = translate(enemyPosX enemy)(enemyPosY enemy)(color yellow (ThickCircle 10 4 ))
                    | enemyValue enemy == (-1) = translate(enemyPosX enemy)(enemyPosY enemy)(color yellow (ThickCircle 10 8 ))
                    | otherwise  = translate(enemyPosX enemy)(enemyPosY enemy)(color yellow (ThickCircle 10 20 ))


deadEnemies :: [Enemy] -> [Enemy] -- ANIMATION FOR DEAD ENEMIES still needs to be implemented
deadEnemies enemies = filter (\x -> (enemyHealth x) <0) enemies

drawPlayer :: Player -> Picture
drawPlayer player = translate (positionX player)(positionY player) (color blue (ThickCircle 15 30))