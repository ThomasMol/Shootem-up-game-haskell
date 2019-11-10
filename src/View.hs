module View where

import Graphics.Gloss

import Model

view :: GameState -> IO Picture
view = return . viewPure

viewPure :: GameState -> Picture
viewPure (GameState player enemies bullets time score) = pictures help
                                                where 
<<<<<<< Updated upstream
                                                    help = [scale 0.4 0.4 (translate (800) 600 (color white (text (show time)))),translate (positionX player) (positionY player) (color white (shape player))] ++ enemydrawing ++ bulletdrawing
                                                    enemydrawing = drawEnemies (GameState player enemies bullets time score)
                                                    bulletdrawing = drawBullets (GameState player enemies bullets time score)
=======
                                                    help = [scale 0.4 0.4 (translate (800) 600 (color white (text (show score)))),translate (positionX player) (positionY player) (color white (shape player))]  ++ enemydrawing ++ bulletdrawing
                                                    enemydrawing = drawEnemies (GameState player enemies bullets time score pause)
                                                    bulletdrawing = drawBullets (GameState player enemies bullets time score pause)
>>>>>>> Stashed changes

drawBullets :: GameState -> [Picture]
drawBullets gstate = map drawBullet (bullets gstate)

drawBullet :: Bullet -> Picture
drawBullet bullet = translate(bulletPosX bullet)(bulletPosY bullet)(color white (ThickCircle 2 4))


drawEnemies :: GameState -> [Picture]
drawEnemies gstate = map drawEnemy  (enemies gstate)

drawEnemy :: Enemy -> Picture
drawEnemy enemy = translate(enemyPosX enemy)(enemyPosY enemy)(color red (ThickCircle 10 20 ))