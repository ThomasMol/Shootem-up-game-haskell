module Controller where

import Model
    
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Random
import GHC.Float
import System.Exit

-- | Handle one iteration of the game
-- move enemies ,add enemies, movebullets , player Input(checkPause, checkPlayerShoot, checkMove), enemyShoot, 
-- checkPlayerHit all bullets, checkEnemyHit all bullets ,removeDead, removeBullets that hit, 
step :: Float -> GameState -> IO GameState
<<<<<<< Updated upstream
step secs gstate =      return $ 
                        timeAdd $
                        moveEnemies $
                        addEnemies $
                        moveBullets gstate
                        {-enemyShoot  $
                        checkPlayerHit $
                        checkEnemyHit   $
                        removeDead  $
                        removeBullets gstate
                        -}
                        


=======
step secs gstate@(GameState player enemies bullets time score Menu {menuItem = x}) =  return $ gstate
step secs gstate@(GameState player enemies bullets time score Highscores) =  return $ gstate
step secs gstate@(GameState player enemies bullets time score Paused) =  return $ gstate    
step secs gstate@(GameState player enemies bullets time score Quit) =  exitSuccess

step secs gstate@(GameState player enemies bullets time score Playing) =                 
                                        return $ 
                                        timeAdd $
                                        moveEnemies $
                                        addEnemies $
                                        moveBullets gstate
                                        {-enemyShoot  $
                                        checkPlayerHit $
                                        checkEnemyHit   $
                                        removeDead  $
                                        removeBullets gstate
                                        -}                    
>>>>>>> Stashed changes


input :: Event -> GameState -> IO GameState
input e gstate = return (inputKey e gstate)

inputKey :: Event -> GameState -> GameState
<<<<<<< Updated upstream
inputKey (EventKey (SpecialKey c) Down _ _) (GameState player enemies bullets time score ) | c == KeyUp = GameState player {positionY = positionY player + 10} enemies bullets time score
                                                                      | c == KeyDown = GameState player {positionY = positionY player - 10} enemies bullets time score
                                                                      | c == KeySpace = GameState player enemies newbullets time score
                                                                      where
                                                                        newbullets = bullets ++ [Bullet{bulletShape = circle 5,bulletPosX = positionX player + 35, bulletPosY = positionY player,
                                                                        bulletRight = True, bulletSpeed = 10 }]
=======
--Game playing or paused input
inputKey (EventKey (SpecialKey KeyUp) Down _ _) (GameState player enemies bullets time score Playing) = GameState player {positionY = positionY player + 10} enemies bullets time score Playing
inputKey (EventKey (SpecialKey KeyDown) Down _ _) (GameState player enemies bullets time score Playing) = GameState player {positionY = positionY player - 10} enemies bullets time score Playing
inputKey (EventKey (SpecialKey KeySpace) Down _ _) (GameState player enemies bullets time score Playing) = GameState player enemies newbullets time score Playing
                                                                                        where
                                                                                            newbullets = bullets ++ [Bullet{bulletShape = circle 5,bulletPosX = positionX player + 35, bulletPosY = positionY player,
                                                                                            bulletRight = True, bulletSpeed = 10, bulletDamage = 15 }]
inputKey (EventKey (Char 'p') Down _ _) (GameState player enemies bullets time score status) = GameState player enemies bullets time score (isPause status)

--Game in menu input
inputKey (EventKey (SpecialKey KeyUp) Down _ _) (GameState player enemies bullets time score Menu {menuItem = x}) | x >= 1 =  GameState player enemies bullets time score Menu {menuItem = x - 1} 
                                                                                                                  | otherwise = GameState player enemies bullets time score Menu {menuItem = x}

inputKey (EventKey (SpecialKey KeyDown) Down _ _) (GameState player enemies bullets time score Menu {menuItem = x}) | x <= 1 =  GameState player enemies bullets time score Menu {menuItem = x + 1}
                                                                                                                    | otherwise =  GameState player enemies bullets time score Menu {menuItem = x }

inputKey (EventKey (SpecialKey KeySpace) Down _ _) (GameState player enemies bullets time score Menu {menuItem = x}) | x == 0 = GameState player enemies bullets time score Playing
                                                                                                                     | x == 1 = GameState player enemies bullets time score Highscores
                                                                                                                     | x == 2 = GameState player enemies bullets time score Quit
                                                                                                                     | otherwise = GameState player enemies bullets time score Menu {menuItem = x}
--Highscore screen input
inputKey (EventKey (SpecialKey KeySpace) Down _ _) (GameState player enemies bullets time score Highscores) = GameState player enemies bullets time score Menu {menuItem = 0}
                                                                     
>>>>>>> Stashed changes

inputKey _ gstate = gstate -- Otherwise keep the same


timeAdd :: GameState -> GameState
timeAdd gstate = gstate { time = (time gstate)+1}

moveEnemies :: GameState -> GameState
moveEnemies gstate = gstate { enemies = map newEnemyPos (enemies gstate) }

newEnemyPos :: Enemy -> Enemy
newEnemyPos enemy = enemy { enemyPosX = enemyPosX enemy - enemySpeed enemy }


addEnemies :: GameState -> GameState
addEnemies gstate   | oi == 0 = addEnemies2 gstate
                    | otherwise = gstate
                    where 
                        test = (time gstate)
                        oi = test `mod` 15


addEnemies2 :: GameState -> GameState
addEnemies2 gstate = gstate {enemies = enemies gstate ++ [Enemy {enemyShape = circle 20, enemyPosX = 300, enemyPosY = random, enemyHealth = 100 , enemySpeed = 10}]
                            }
                    where
                        --random = genRange(-600,600)

                        random = int2Float ((((time gstate)+1) `mod` 30 ) * 10)
                        --random = randomRIO (-600,600)



moveBullets :: GameState -> GameState
moveBullets gstate = gstate { bullets = map newBullets (bullets gstate) }

newBullets :: Bullet -> Bullet
newBullets bullet = bullet {bulletPosX = placeholder  }
                    where
                        placeholder | bulletRight bullet = bulletPosX bullet + bulletSpeed bullet
                                    | otherwise = bulletPosX bullet -bulletSpeed bullet --movement naar links moet nog een extra waarde krijgen

enemyShoot  :: GameState -> GameState
enemyShoot  = undefined

checkPlayerHit  :: GameState -> GameState
checkPlayerHit  = undefined

checkEnemyHit   :: GameState -> GameState
checkEnemyHit   = undefined


removeDead  :: GameState -> GameState
removeDead  = undefined

removeBullets   :: GameState -> GameState
removeBullets   = undefined

<<<<<<< Updated upstream
=======
                        
isPause :: Status -> Status
isPause Playing = Paused 
isPause Paused = Playing
>>>>>>> Stashed changes
