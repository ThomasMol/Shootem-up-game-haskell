module View where

import Graphics.Gloss
import Control.Exception
import System.IO
import Model
import System.Directory
import Control.Monad
import GHC.Float



view :: GameState -> IO Picture
view gstate@(GameState player enemies bullets time score Highscores) = do
                                                                        contents <- (readFile "highscores.txt") `catch` readHandler
                                                                        return (pictures (drawHighscores (lines contents)))
view gstate@(GameState player enemies bullets time score Dead {saved = x}) = return (pictures drawDead)                                                                                   
view gstate@(GameState player enemies bullets time score status) = return (viewPure gstate)

readHandler:: IOError -> IO String
readHandler e = return "Could not find any highscores..."

viewPure :: GameState -> Picture
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
                                                    help = [scale 0.4 0.4 (translate (800) 600 (color white (text (show score)))),translate (positionX player) (positionY player) (color white (shape player))] ++ enemydrawing ++ bulletdrawing
                                                    enemydrawing = drawEnemies (GameState player enemies bullets time score Playing)
                                                    bulletdrawing = drawBullets (GameState player enemies bullets time score Playing)

drawMenuIndicators :: Float -> Picture
drawMenuIndicators x = scale 0.2 0.2 (translate (-1200) ((x*(-150)) + 300) (color white (text ">" )))

drawHighscores :: [String] -> [Picture]
drawHighscores xs = [scale 0.2 0.2 (translate (-1000) 300 (color white (text "Press space to go back."))),scale 0.2 0.2 (translate (-1000) 150 (color blue (text "Highscores:")))] ++ listedScores                   
                    where scores = [(color white (text x)) |x <- xs] 
                          ypositions = [int2Float(y*(-150)) + 150  | y <- [1..(length xs)]]   
                          listedScores = zipWith (\x y -> scale 0.2 0.2 (translate (-1000) y x)) scores ypositions
                   

drawDead :: [Picture]
drawDead = [scale 0.2 0.2 (translate (-1000) 300 (color white (text "Press space to go to the menu."))),scale 0.2 0.2 (translate (-1000) 150 (color red (text "YOU'RE DEAD")))]


drawBullets :: GameState -> [Picture]
drawBullets gstate = map drawBullet (bullets gstate)

drawBullet :: Bullet -> Picture
drawBullet bullet = translate(bulletPosX bullet)(bulletPosY bullet)(color white (ThickCircle 2 4))

drawEnemies :: GameState -> [Picture]
drawEnemies gstate = map drawEnemy  (enemies gstate)

drawEnemy :: Enemy -> Picture
drawEnemy enemy = translate(enemyPosX enemy)(enemyPosY enemy)(color red (ThickCircle 10 20 ))
