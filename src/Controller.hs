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
-- move enemies ,add enemies, movebullets , player Input(checkPause, checkPlayerShoot, checkMove), enemyShoot, 
-- checkPlayerHit all bullets, checkEnemyHit all bullets ,removeDead, removeBullets that hit, 
step :: Float -> GameState -> IO GameState
step secs gstate@(GameState player enemies bullets time score Menu {menuItem = x}) =  return $ gstate
step secs gstate@(GameState player enemies bullets time score Highscores) =  return $ timeAdd $ gstate
step secs gstate@(GameState player enemies bullets time score Paused) =  return $ gstate    
step secs gstate@(GameState player enemies bullets time score Dead {saved = x}) =  return $ gstate  

step secs gstate@(GameState player enemies bullets time score Quit) =  exitSuccess

step secs gstate@(GameState player enemies bullets time score Playing) =                 
                        addEnemies $ 
                        timeAdd $
                        moveEnemies $
                        removeBullets $
                        moveBullets $
                        checkAllEnemies $
                        --enemyShoot $ Crasht snel
                        checkPlayerHit $
                        removeDead 
                          gstate
                        {-enemyShoot  $
                        checkPlayerHit $
                        checkPlayerHealth $
                        checkEnemyHit   $
                         gstate
                        -}                  



input :: Event -> GameState -> IO GameState
input e gstate@(GameState player enemies bullets time score Dead {saved = x}) | not x = do
                                                                                    let fileName = "highscores.txt"
                                                                                    fileExists <- doesFileExist fileName
                                                                                    if not fileExists
                                                                                        then do writeFile fileName ((show score)++"\n")
                                                                                        else do file <- readFile fileName
                                                                                                let newContent = (show score)++"\n" ++ file
                                                                                                when (length newContent > 0) $
                                                                                                    writeFile fileName newContent 
                                                                                    return (inputKey e (GameState player enemies bullets time score Dead {saved = True}))
                                                                               | otherwise = return (inputKey e gstate)                     
input e gstate@(GameState player enemies bullets time score status) =  return (inputKey e gstate)


inputKey :: Event -> GameState -> GameState

--Game playing or paused input
inputKey (EventKey (SpecialKey KeyUp) Down _ _) (GameState player enemies bullets time score Playing) = GameState player {positionY = positionY player + 10} enemies bullets time score Playing
inputKey (EventKey (SpecialKey KeyDown) Down _ _) (GameState player enemies bullets time score Playing) = GameState player {positionY = positionY player - 10} enemies bullets time score Playing
inputKey (EventKey (SpecialKey KeySpace) Down _ _) (GameState player enemies bullets time score Playing) = GameState player enemies newbullets time score Playing
                                                                                        where
                                                                                            newbullets = bullets ++ [Bullet{bulletShape = circle 4,bulletPosX = positionX player + 35, bulletPosY = positionY player,
                                                                                            bulletRight = True, bulletSpeed = 30, bulletDamage = 15, bulletHit = False }]
inputKey (EventKey (Char 'p') Down _ _) (GameState player enemies bullets time score status) = GameState player enemies bullets time score (isPause status)

--Game in menu input
inputKey (EventKey (SpecialKey KeyUp) Down _ _) (GameState player enemies bullets time score Menu {menuItem = x}) | x >= 1 =  GameState player enemies bullets time score Menu {menuItem = x - 1} 
                                                                                                                  | otherwise = GameState player enemies bullets time score Menu {menuItem = x}

inputKey (EventKey (SpecialKey KeyDown) Down _ _) (GameState player enemies bullets time score Menu {menuItem = x}) | x <= 1 =  GameState player enemies bullets time score Menu {menuItem = x + 1}
                                                                                                                    | otherwise =  GameState player enemies bullets time score Menu {menuItem = x }

inputKey (EventKey (SpecialKey KeySpace) Down _ _) (GameState player enemies bullets time score Menu {menuItem = x}) | x == 0 = newGameState
                                                                                                                     | x == 1 = GameState player enemies bullets 0 score Highscores
                                                                                                                     | x == 2 = GameState player enemies bullets 0 score Quit
                                                                                                                     | otherwise = GameState player enemies bullets 0 score Menu {menuItem = x}
--Dead screen input
inputKey (EventKey (SpecialKey KeySpace) Down _ _) (GameState player enemies bullets time score Dead{saved = x}) = GameState player enemies bullets 0 score Menu {menuItem = 0}


--Highscore screen input
inputKey (EventKey (SpecialKey KeySpace) Down _ _) (GameState player enemies bullets time score Highscores) = GameState player enemies bullets 0 score Menu {menuItem = 0}
                                                                  

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
                            fromEnemy = not (bulletRight bullet)
                            dx = (bulletPosX bullet) - (positionX player)
                            dy = (bulletPosY bullet) - (positionY player)
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


 -------------------------------------------------------------------ANIMATION----------------------------------------------------------------











-----------------------------------------------------------------------OTHER ----------------------------------------------------------------------------

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
remo2 bullets = filter (\x -> not(bulletHit x)) bullets
                       
                       
isPause :: Status -> Status
isPause Playing = Paused 
isPause Paused = Playing
