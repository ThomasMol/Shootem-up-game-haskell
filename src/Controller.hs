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
step secs gstate |  isPause (pause gstate)  = 
                
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
                        |otherwise =  return $ gstate


input :: Event -> GameState -> IO GameState
input e gstate = return (inputKey e gstate)

inputKey :: Event -> GameState -> GameState
inputKey (EventKey (SpecialKey c) Down _ _) (GameState player enemies bullets time score pause ) |(pause == False && c == KeyUp) = GameState player {positionY = positionY player + 10} enemies bullets time score pause
                                                                      | pause == False && c == KeyDown = GameState player {positionY = positionY player - 10} enemies bullets time score pause
                                                                      | pause == False && c == KeySpace = GameState player enemies newbullets time score pause
                                                                      | c == KeyDelete = GameState player enemies bullets time score (isPause pause)
                                                                      where
                                                                        newbullets = bullets ++ [Bullet{bulletShape = circle 5,bulletPosX = positionX player + 35, bulletPosY = positionY player,
                                                                        bulletRight = True, bulletSpeed = 10, bulletDamage = 15 }]

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

                        
isPause :: Bool -> Bool
isPause x   | x = False
            | otherwise = True
