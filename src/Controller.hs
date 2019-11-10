module Controller where

import Model
    
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Random
import GHC.Float

-- | Handle one iteration of the game
-- move enemies ,add enemies, movebullets , player Input(checkPause, checkPlayerShoot, checkMove), enemyShoot, 
-- checkPlayerHit all bullets, checkEnemyHit all bullets ,removeDead, removeBullets that hit, 
step :: Float -> GameState -> IO GameState
<<<<<<< Updated upstream
step secs gstate =      return $ 
=======
step secs gstate |pause gstate  = return $ gstate
                 |otherwise =  
                                        
                        return $ 
>>>>>>> Stashed changes
                        timeAdd $
                        removeBullets $
                        moveEnemies $
                        addEnemies $
                        moveBullets $
                        checkAllEnemies $
                        enemyShoot $
                        checkPlayerHit $
                        removeDead 
                          gstate
                        {-enemyShoot  $
                        checkPlayerHit $
                        checkPlayerHealth $
                        checkEnemyHit   $
                         gstate
                        -}
<<<<<<< Updated upstream
                        


=======
>>>>>>> Stashed changes


input :: Event -> GameState -> IO GameState
input e gstate = return (inputKey e gstate)

inputKey :: Event -> GameState -> GameState
inputKey (EventKey (SpecialKey c) Down _ _) (GameState player enemies bullets time score ) | c == KeyUp = GameState player {positionY = positionY player + 10} enemies bullets time score
                                                                      | c == KeyDown = GameState player {positionY = positionY player - 10} enemies bullets time score
                                                                      | c == KeySpace = GameState player enemies newbullets time score
                                                                      where
<<<<<<< Updated upstream
                                                                        newbullets = bullets ++ [Bullet{bulletShape = circle 5,bulletPosX = positionX player + 35, bulletPosY = positionY player,
                                                                        bulletRight = True, bulletSpeed = 10 }]
=======
                                                                        newbullets = bullets ++ [Bullet{bulletShape = circle 4,bulletPosX = positionX player + 35, bulletPosY = positionY player,
                                                                        bulletRight = True, bulletSpeed = 30, bulletDamage = 15, bulletHit = False }]
                                                                        
>>>>>>> Stashed changes

inputKey _ gstate = gstate -- Otherwise keep the same


timeAdd :: GameState -> GameState
timeAdd gstate = gstate { time = (time gstate)+1}

moveBullets :: GameState -> GameState
moveBullets gstate = gstate { bullets = map newBullets (bullets gstate) }

newBullets :: Bullet -> Bullet
newBullets bullet = bullet {bulletPosX = placeholder  }
                    where
                        placeholder | bulletRight bullet = bulletPosX bullet + bulletSpeed bullet
                                    | otherwise = bulletPosX bullet -bulletSpeed bullet --movement naar links moet nog een extra waarde krijgen






-------------------------------------------------------------------------Player CODE --------------------------------------------------------------------------------------



checkPlayerHit  :: GameState -> GameState
checkPlayerHit gstate = gstate { player = hitcheck1, bullets = hitcheck2}
                        where 
                            hitcheck1 = playerHP (bullets gstate) (player gstate) 
                            hitcheck2 = playerNewBulletHit (bullets gstate) (player gstate)
                            

playerNewBulletHit :: [Bullet] -> Player -> [Bullet]
playerNewBulletHit [] player = []
playerNewBulletHit (b:lt) player    | (bulletPlayerHit player b)  = [ b { bulletHit = True}] ++ (playerNewBulletHit lt player)
                                    | otherwise = [b] ++ (playerNewBulletHit lt player)


-- reduce health from player
playerHP ::  [Bullet] -> Player -> Player
playerHP [] player = player
playerHP (b:lt) player      | (bulletPlayerHit player b) = playerHP lt newPlayer 
                            | otherwise = playerHP lt player
                            where
                                newPlayer = player{ health = (health player) - (bulletDamage b)}



bulletPlayerHit :: Player -> Bullet -> Bool
bulletPlayerHit player bullet = (distance <(30 +4) && fromEnemy) 
                        where
                            fromEnemy = isPause (bulletRight bullet)
                            dx = (bulletPosX bullet) - (positionX player)
                            dy = (bulletPosY bullet) - (positionY player)
                            distance = sqrt(dx*dx + dy*dy)



--------------------------------------------------------------------------ENEMY CODE -------------------------------------------------------------------------------

moveEnemies :: GameState -> GameState
moveEnemies gstate = gstate { enemies = map newEnemyPos (enemies gstate) }

newEnemyPos :: Enemy -> Enemy
newEnemyPos enemy = enemy { enemyPosX = enemyPosX enemy - enemySpeed enemy }


addEnemies :: GameState -> GameState -- RANDOM SPAWN MOET NOG GEIMPLEMENTEERD WORDEN
addEnemies gstate   | oi == 0 = addEnemies2 gstate
                    | otherwise = gstate
                    where 
                        test = (time gstate)
                        oi = test `mod` 15


addEnemies2 :: GameState -> GameState
addEnemies2 gstate = gstate {enemies = enemies gstate ++ [Enemy {enemyShape = circle 20, enemyPosX = 300, enemyPosY = random, enemyHealth = 100 , enemySpeed = 5, enemyValue = 10}]
                            }
                    where
                        --random = genRange(-600,600)

                        random = int2Float ((((time gstate)+1) `mod` 30 ) * 10)
                        --random = randomRIO (-600,600)


checkAllEnemies :: GameState -> GameState
checkAllEnemies gstate = gstate{ enemies = hitcheck1, bullets = hitcheck2}
                            where
                                hitcheck1  = map (enemyHP(bullets gstate))(enemies gstate) -- reduce health from enemies
                                hitcheck2  = map (enemyNewBulletHit (enemies gstate)) (bullets gstate) -- toggle bulletHit      


enemyNewBulletHit :: [Enemy] -> Bullet -> Bullet
enemyNewBulletHit  [] bullet = bullet
enemyNewBulletHit (n:mi) bullet | (bulletEnemyHit n bullet) = bullet{ bulletHit = True}
                                | otherwise = enemyNewBulletHit mi bullet 


enemyHP ::  [Bullet] -> Enemy -> Enemy
enemyHP [] enemy = enemy
enemyHP (b:lt) enemy    | (bulletEnemyHit enemy b) = enemyHP lt newEnemy 
                        | otherwise = enemyHP lt enemy

                            where
                                newEnemy = enemy{ enemyHealth = (enemyHealth enemy) - (bulletDamage b)}


bulletEnemyHit :: Enemy -> Bullet -> Bool
bulletEnemyHit enemy bullet = (distance < (20 + 4)) && fromPlayer  -- bullet and enemy size are fixed atm

                        where
                            fromPlayer = bulletRight bullet
                            dx = ((bulletPosX bullet) - (enemyPosX enemy))
                            dy = ((bulletPosY bullet) - (enemyPosY enemy))
                            distance = sqrt(dx*dx + dy*dy)


enemyShoot :: GameState -> GameState
enemyShoot gstate = gstate { bullets = oi} -- gstate{ bullets = shoot}
                                where
                                    wat = time gstate 
                                    oi = drawEnemybullet gstate (enemies gstate)


drawEnemybullet :: GameState -> [Enemy] ->  [Bullet]
drawEnemybullet gstate [] = []
drawEnemybullet gstate (n:mi) | ((time gstate) `mod` 15 == 0) = (bullets gstate) ++ [Bullet{bulletShape = circle 4,bulletPosX = (enemyPosX n) - 25, bulletPosY = (enemyPosY n),
                                             bulletRight = False, bulletSpeed = 10, bulletDamage = 10, bulletHit = False }] ++ (drawEnemybullet gstate mi)
                                             | otherwise = bullets gstate




--------------------------------------------------------------------------------------------------------OTHER ----------------------------------------------------------------------------

removeDead  :: GameState -> GameState -- remove dead enemies & update player score
removeDead gstate = gstate { enemies = test, score = (score gstate + test2) }
                            where
                                test = remo (enemies gstate)
                                test2 = updateScore (deadEnemies (enemies gstate))

remo :: [Enemy] -> [Enemy]
remo enemies = filter (\x -> (enemyHealth x) >0) enemies

deadEnemies :: [Enemy] -> [Enemy] -- ANIMATION FOR DEAD ENEMIES still needs to be implemented
deadEnemies enemies = filter (\x -> (enemyHealth x) <0) enemies

updateScore :: [Enemy] -> Int
updateScore [] = 0
updateScore (n:mi) = enemyValue n + updateScore mi

removeBullets   :: GameState -> GameState
removeBullets gstate = gstate { bullets = test2 }
                where
                        test2 = remo2 (bullets gstate) -- remove every bullet where bulletHit = True

remo2 :: [Bullet] -> [Bullet]
remo2 bullets = filter (\x -> isPause(bulletHit x)) bullets

<<<<<<< Updated upstream
=======
                       
-- turn True into false and vice versa
isPause :: Bool -> Bool
isPause x   | x = False
            | otherwise = True
>>>>>>> Stashed changes
