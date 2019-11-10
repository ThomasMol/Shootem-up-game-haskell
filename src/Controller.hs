module Controller where

import Model
    
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Random
import GHC.Float
import System.Exit
import System.IO
import System.Directory
import Control.Monad

-- | Handle one iteration of the game
step :: Float -> GameState -> IO GameState

step secs gstate@(GameState player enemies bullets time score animations Menu {menuItem = x}) =  return $ gstate
step secs gstate@(GameState player enemies bullets time score animations Highscores) =  return $ timeAdd $ gstate
step secs gstate@(GameState player enemies bullets time score animations Paused) =  return $ gstate    
step secs gstate@(GameState player enemies bullets time score animations Dead {saved = x}) =  return $ gstate  

step secs gstate@(GameState player enemies bullets time score animations Quit) =  exitSuccess

step secs gstate@(GameState player enemies bullets time score animations Playing) =                 
                        addEnemies $ 

                        timeAdd $
                        moveEnemies $
                        enemyOOB $
                        bulletsOOB $
                        removeBullets $
                        moveBullets $
                        checkAllEnemies $
                        enemyShoot $ 
                        checkPlayerHit $
                        checkPlayerDead $
                        removeDead 
                          gstate

                          




input :: Event -> GameState -> IO GameState
input e gstate@(GameState player enemies bullets time score animations Dead {saved = x}) | not x = do
                                                                                    let fileName = "highscores.txt"
                                                                                    fileExists <- doesFileExist fileName
                                                                                    if not fileExists
                                                                                        then do writeFile fileName ((show score)++"\n")
                                                                                        else do file <- readFile fileName
                                                                                                let newContent = (show score)++"\n" ++ file
                                                                                                when (length newContent > 0) $
                                                                                                    writeFile fileName newContent 
                                                                                    return (inputKey e (GameState player enemies bullets time score animations Dead {saved = True}))
                                                                               | otherwise = return (inputKey e gstate)                     
input e gstate@(GameState player enemies bullets time score animations status) =  return (inputKey e gstate)


inputKey :: Event -> GameState -> GameState

--Game playing or paused input
inputKey (EventKey (SpecialKey KeyUp) Down _ _) (GameState player enemies bullets time score animations  Playing)   | (positionY player > 200) = GameState player {positionY = positionY player} enemies bullets time score animations Playing
                                                                                                                    | otherwise =  GameState player {positionY = positionY player + 20} enemies bullets time score animations Playing
inputKey (EventKey (SpecialKey KeyDown) Down _ _) (GameState player enemies bullets time score animations Playing)  | (positionY player) < (-250) = GameState player {positionY = positionY player} enemies bullets time score animations Playing
                                                                                                                    | otherwise =  GameState player {positionY = positionY player - 20} enemies bullets time score animations Playing
inputKey (EventKey (SpecialKey KeySpace) Down _ _) (GameState player enemies bullets time score animations Playing) = GameState player enemies newbullets time score animations Playing
                                                                                        where
                                                                                            newbullets = bullets ++ [Bullet{bulletShape = circle 4,bulletPosX = positionX player + 35, bulletPosY = positionY player,
                                                                                            bulletRight = True, bulletSpeed = 30, bulletDamage = 15, bulletHit = False }]
inputKey (EventKey (Char 'p') Down _ _) (GameState player enemies bullets time score animations status) = GameState player enemies bullets time score animations (isPause status)

--Game in menu input
inputKey (EventKey (SpecialKey KeyUp) Down _ _) (GameState player enemies bullets time score animations Menu {menuItem = x}) | x >= 1 =  GameState player enemies bullets time score animations Menu {menuItem = x - 1} 
                                                                                                                  | otherwise = GameState player enemies bullets time score animations Menu {menuItem = x}

inputKey (EventKey (SpecialKey KeyDown) Down _ _) (GameState player enemies bullets time score animations Menu {menuItem = x}) | x <= 1 =  GameState player enemies bullets time score animations Menu {menuItem = x + 1}
                                                                                                                    | otherwise =  GameState player enemies bullets time score animations Menu {menuItem = x }

inputKey (EventKey (SpecialKey KeySpace) Down _ _) (GameState player enemies bullets time score animations Menu {menuItem = x}) | x == 0 = newGameState
                                                                                                                     | x == 1 = GameState player enemies bullets 0 score animations Highscores
                                                                                                                     | x == 2 = GameState player enemies bullets 0 score animations Quit
                                                                                                                     | otherwise = GameState player enemies bullets 0 score animations Menu {menuItem = x}
--Dead screen input
inputKey (EventKey (SpecialKey KeySpace) Down _ _) (GameState player enemies bullets time score animations Dead{saved = x}) = GameState player enemies bullets 0 score animations Menu {menuItem = 0}


--Highscore screen input
inputKey (EventKey (SpecialKey KeySpace) Down _ _) (GameState player enemies bullets time score animations Highscores) = GameState player enemies bullets 0 score animations Menu {menuItem = 0}
                                                                  
inputKey _ gstate = gstate -- Otherwise keep the same

timeAdd :: GameState -> GameState
timeAdd gstate = gstate { time = (time gstate)+1}

moveBullets :: GameState -> GameState
moveBullets gstate = gstate { bullets = map newBullets (bullets gstate) }

newBullets :: Bullet -> Bullet
newBullets bullet = bullet {bulletPosX = placeholder  }
                    where
                        placeholder | bulletRight bullet = bulletPosX bullet + bulletSpeed bullet
                                    | otherwise = bulletPosX bullet -bulletSpeed bullet 


-------------------------------------------------------------------------Player CODE --------------------------------------------------------------------------------------
checkPlayerDead :: GameState -> GameState
checkPlayerDead gstate  | health (player gstate) <=0 = gstate {status = Dead {saved = False}}
                        | otherwise = gstate


checkPlayerHit  :: GameState -> GameState
checkPlayerHit gstate = gstate { player = hitcheck1, bullets = hitcheck2}
                        where 
                            hitcheck1 = playerHP (bullets gstate) (enemies gstate) (player gstate) 
                            hitcheck2 = playerNewBulletHit (bullets gstate) (player gstate)
                            

playerNewBulletHit :: [Bullet] -> Player -> [Bullet]
playerNewBulletHit [] player = []
playerNewBulletHit (b:lt) player    | (bulletPlayerHit player b)  = [ b { bulletHit = True}] ++ (playerNewBulletHit lt player)
                                    | otherwise = [b] ++ (playerNewBulletHit lt player)


-- reduce health from player
playerHP :: [Bullet] -> [Enemy] -> Player -> Player
playerHP [] [] player = player
playerHP [] (n:mi) player   | (enemyPlayerHit player n) = playerHP [] mi newPlayer3
                            | otherwise = playerHP [] mi player
                            where
                                newPlayer3 = player{ health = (health player) - 30}

playerHP (b:lt) [] player   | (bulletPlayerHit player b) = playerHP lt [] newPlayer
                            | otherwise = playerHP lt [] player
                            where
                                newPlayer = player{ health = (health player) - (bulletDamage b)}

playerHP (b:lt) (n:mi) player   | (bulletPlayerHit player b) && (enemyPlayerHit player n)  = playerHP lt mi newPlayer 
                                | (bulletPlayerHit player b) = playerHP lt mi newPlayer2
                                | (enemyPlayerHit player n) = playerHP lt mi newPlayer3
                                | otherwise = playerHP lt mi player
                                    where
                                        newPlayer = player{ health = (health player) - (bulletDamage b) - 30}
                                        newPlayer2 = player{ health = (health player) - (bulletDamage b)}
                                        newPlayer3 = player{ health = (health player) - 30}



bulletPlayerHit :: Player -> Bullet -> Bool
bulletPlayerHit player bullet = (distance <(30 +4) && fromEnemy) 
                        where
                            fromEnemy = not (bulletRight bullet)
                            dx = abs ((bulletPosX bullet) - (positionX player))
                            dy = abs((bulletPosY bullet) - (positionY player))
                            distance = sqrt(dx*dx + dy*dy)


                                
enemyPlayerHit :: Player -> Enemy -> Bool
enemyPlayerHit player enemy = (distance <(30 +20) ) 
                        where
                            dx = abs((enemyPosX enemy) - (positionX player))
                            dy = abs((enemyPosY enemy) - (positionY player))
                            distance = sqrt(dx*dx + dy*dy)


--------------------------------------------------------------------------ENEMY CODE -------------------------------------------------------------------------------

moveEnemies :: GameState -> GameState
moveEnemies gstate = gstate { enemies = map newEnemyPos (enemies gstate) }

newEnemyPos :: Enemy -> Enemy
newEnemyPos enemy = enemy { enemyPosX = enemyPosX enemy - enemySpeed enemy }


addEnemies :: GameState -> IO GameState
addEnemies gstate   | spawnChecker == 0 = addEnemies' gstate
                    | otherwise = return gstate
                    where 
                        timeNow = (time gstate)
                        spawnChecker = timeNow `mod` 15

addEnemies' :: GameState -> IO GameState
addEnemies' gstate = do
                    randomnum <- randomRIO((-200),200 :: Float) --generate random float for enemy y position
                    return (gstate {enemies = enemies gstate ++ [Enemy {enemyShape = circle 20, enemyPosX = 300, enemyPosY = randomnum, enemyHealth = 100 , enemySpeed = 5, enemyValue = 10}]})



checkAllEnemies :: GameState -> GameState
checkAllEnemies gstate = gstate{ enemies = hitcheck1, bullets = hitcheck2}
                            where
                                hitcheck1  = map (enemyHP(bullets gstate)(player gstate))(enemies gstate) -- reduce health from enemies
                                hitcheck2  = map (enemyNewBulletHit (enemies gstate)) (bullets gstate) -- toggle bulletHit      


enemyNewBulletHit :: [Enemy] -> Bullet -> Bullet
enemyNewBulletHit  [] bullet = bullet
enemyNewBulletHit (n:mi) bullet | (bulletEnemyHit n bullet) = bullet{ bulletHit = True}
                                | otherwise = enemyNewBulletHit mi bullet 

-- reduce enemy HP
enemyHP ::  [Bullet] -> Player -> Enemy -> Enemy
enemyHP [] player enemy | (enemyPlayerHit player enemy) = enemy{ enemyHealth = (enemyHealth enemy) - 100}
                        | otherwise = enemy

enemyHP (b:lt) player enemy     | (bulletEnemyHit enemy b) && (enemyPlayerHit player enemy) = enemyHP lt player newEnemy1
                                | (bulletEnemyHit enemy b) = enemyHP lt player newEnemy2 
                                | otherwise = enemyHP lt player enemy

                            where
                                newEnemy1 = enemy{ enemyHealth = (enemyHealth enemy) - (bulletDamage b) -100}
                                newEnemy2 = enemy{ enemyHealth = (enemyHealth enemy) - (bulletDamage b)}


bulletEnemyHit :: Enemy -> Bullet -> Bool
bulletEnemyHit enemy bullet = (distance < (20 + 4)) && fromPlayer  -- bullet and enemy size are fixed atm

                        where
                            fromPlayer = bulletRight bullet
                            dx = abs((bulletPosX bullet) - (enemyPosX enemy))
                            dy = abs((bulletPosY bullet) - (enemyPosY enemy))
                            distance = sqrt(dx*dx + dy*dy)


enemyShoot :: GameState -> GameState
enemyShoot gstate   | (time gstate)`mod` 15 == 0  = gstate { bullets = oi} -- gstate{ bullets = shoot}
                    | otherwise = gstate        
                                where
                                oi = (bullets gstate) ++ drawEnemybullet (player gstate) (enemies gstate)

-- Shoot if the player X position is close to that of the enemy 
drawEnemybullet :: Player -> [Enemy] ->  [Bullet]
drawEnemybullet player [] = []
drawEnemybullet player (n:mi)   | abs((enemyPosY n) - (positionY player)) < 60 =  [Bullet{bulletShape = circle 4,bulletPosX = (enemyPosX n) - 25, bulletPosY = (enemyPosY n),
bulletRight = False, bulletSpeed = 10, bulletDamage = 10, bulletHit = False }] ++ (drawEnemybullet player mi)

                                | otherwise = (drawEnemybullet player mi)


-- remove enemies from the list that are out of bounds
enemyOOB :: GameState -> GameState
enemyOOB gstate = gstate { enemies = test}
                where 
                test = remEOOB (enemies gstate)

remEOOB :: [Enemy] -> [Enemy]
remEOOB enemies = filter (\x -> (enemyPosX x) >(-410)) enemies





 -------------------------------------------------------------------ANIMATION----------------------------------------------------------------


-- dode enemies 
updateAnimationTick :: [Enemy] -> [Enemy]
updateAnimationTick [] = []
updateAnimationTick (n:mi)  | enemyValue n == (-1) = [n { enemyValue = (-2)}] ++ updateAnimationTick mi
                            | enemyValue n == (-2) = [n { enemyValue = (-3)}] ++ updateAnimationTick mi
                            | enemyValue n == (-3) = [n { enemyValue = (-4)}] ++ updateAnimationTick mi
                            | otherwise = [n { enemyValue =(-1)}] ++ updateAnimationTick mi
                            


remo3 :: [Enemy] -> [Enemy]
remo3 enemies = filter (\x -> (enemyValue x) >(-4)) enemies


-----------------------------------------------------------------------OTHER ----------------------------------------------------------------------------

removeDead  :: GameState -> GameState -- remove dead enemies & update player score & put dead enemies in the animation list
removeDead gstate = gstate { enemies = test, score = (score gstate + test2), animations = test3 }
                            where
                                test = remo (enemies gstate)
                                test2 = updateScore (deadEnemies (enemies gstate))
                                test4 = (updateAnimationTick ( (animations gstate) ++ deadEnemies(enemies gstate)))
                                test3 = remo3 (test4)

remo :: [Enemy] -> [Enemy]
remo enemies = filter (\x -> (enemyHealth x) >=0) enemies

deadEnemies :: [Enemy] -> [Enemy] -- ANIMATION FOR DEAD ENEMIES still needs to be implemented
deadEnemies enemies = filter (\x -> (enemyHealth x) <=0) enemies

updateScore :: [Enemy] -> Int
updateScore [] = 0
updateScore (n:mi) = enemyValue n + updateScore mi

----------------------------------------------------Bullets--------------------------------------------------------------
removeBullets   :: GameState -> GameState
removeBullets gstate = gstate { bullets = test2 }
                where
                        test2 = remo2 (bullets gstate) -- remove every bullet where bulletHit = True

remo2 :: [Bullet] -> [Bullet]
remo2 bullets = filter (\x -> not(bulletHit x)) bullets

bulletsOOB :: GameState -> GameState
bulletsOOB gstate  = gstate { bullets = test}
                    where 
                    test = remBOOB (bullets gstate)

remBOOB :: [Bullet] -> [Bullet]
remBOOB bullets = filter (\x -> (bulletPosX x) >(-410) && (bulletPosX x) < 410 ) bullets
                       
isPause :: Status -> Status
isPause Playing = Paused 
isPause Paused = Playing
