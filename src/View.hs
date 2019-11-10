module View where

import Graphics.Gloss
import Control.Exception
import System.IO

import Model

view :: GameState -> IO Picture
view gstate@(GameState player enemies bullets time score status) | status == Highscores = do
                                                                                        contents <- (readFile "highscores.txt") `catch` readHandler
                                                                                        return (pictures (drawHighscores (lines contents)))                                                                                         
                                                                 | otherwise =  return (viewPure gstate)
readHandler:: IOError -> IO String
readHandler e = return "Could not find any highscores..."

viewPure :: GameState -> Picture
<<<<<<< Updated upstream
viewPure (GameState player enemies bullets time score) = pictures help
                                                where 
                                                    help = [scale 0.4 0.4 (translate (800) 600 (color white (text (show time)))),translate (positionX player) (positionY player) (color white (shape player))] ++ enemydrawing ++ bulletdrawing
                                                    enemydrawing = drawEnemies (GameState player enemies bullets time score)
                                                    bulletdrawing = drawBullets (GameState player enemies bullets time score)
=======

--Game in Menu
viewPure (GameState player enemies bullets time score Menu {menuItem = x}) = pictures [scale 0.2 0.2 (translate (-1000) 300 (color white (text "New Game"))),
                                                                                        scale 0.2 0.2 (translate (-1000) 150 (color white (text "Highscores"))),
                                                                                        scale 0.2 0.2 (translate (-1000) 0 (color white (text "Quit"))),
                                                                                        drawMenuIndicators x]
--Game is paused
viewPure (GameState player enemies bullets time score Paused) = pictures [scale 0.2 0.2 (translate (-1000) 300 (color white (text "Game is paused, press p again to unpause"))),
                                                                            translate (positionX player) (positionY player) (color white (shape player))]
--Game is playing
viewPure (GameState player enemies bullets time score Playing) = pictures help
                                                where 
                                                    help = [scale 0.4 0.4 (translate (800) 600 (color white (text (show time)))),translate (positionX player) (positionY player) (color white (shape player))] ++ enemydrawing ++ bulletdrawing
                                                    enemydrawing = drawEnemies (GameState player enemies bullets time score Playing)
                                                    bulletdrawing = drawBullets (GameState player enemies bullets time score Playing)

drawMenuIndicators :: Float -> Picture
drawMenuIndicators x = scale 0.2 0.2 (translate (-1200) ((x*(-150)) + 300) (color white (text ">" )))

drawHighscores :: [String] -> [Picture]
drawHighscores xs = [scale 0.2 0.2 (translate (-1000) 300 (color white (text "Press space to go back."))),scale 0.2 0.2 (translate (-1000) 150 (color blue (text "Highscores:")))] ++ map f xs
        where f x = scale 0.2 0.2 (translate (-1000) 0 (color white (text x)))                                   

>>>>>>> Stashed changes

drawBullets :: GameState -> [Picture]
drawBullets gstate = map drawBullet (bullets gstate)

drawBullet :: Bullet -> Picture
drawBullet bullet = translate(bulletPosX bullet)(bulletPosY bullet)(color blue (bulletShape bullet))

drawEnemies :: GameState -> [Picture]
drawEnemies gstate = map drawEnemy  (enemies gstate)

drawEnemy :: Enemy -> Picture
drawEnemy enemy = translate(enemyPosX enemy)(enemyPosY enemy)(color red (enemyShape enemy))