module Model where
import Graphics.Gloss

data Player = Player {  shape     :: Shape,
                        positionX :: Position,
                        positionY :: Position,
                        health    :: Health
                     }
                     
data Enemy = Enemy {  enemyShape      :: Shape,
                      enemyPosX       :: Position,
                      enemyPosY       :: Position,
                      enemyHealth     :: Health,
                      enemySpeed      :: Speed
                    }        
data Bullet = Bullet { bulletShape    :: Shape,
                      bulletPosX      :: Position,
                      bulletPosY      :: Position,
                      bulletRight     :: Bool,
                      bulletSpeed     :: Speed
                    }                 

data Status = Playing | Paused | Dead | Menu {menuItem :: Float}| Highscores | Quit deriving (Eq)


{- data Enemy      = Shape Position Health Speed HitCheck Value
data Bullet	    = Shape Position Speed Direction Damage HitCheck Sender -}
--data Direction  = Left | Right

-- direction Right = 0 , Direction Left = 1
--type Direction = Int

type Shape      = Picture --change later to bitmap 
type Health     = Int
type Speed      = Float
type Damage     = Int
type Position   = Float

data GameState = GameState  { player  :: Player,
                              enemies :: [Enemy],
                              bullets :: [Bullet],
                              time    :: Int, 
<<<<<<< Updated upstream
                              score   :: Int
=======
                              score   :: Int,
                              status  :: Status
>>>>>>> Stashed changes
                            }


initialState :: GameState
<<<<<<< Updated upstream
initialState = GameState (Player {shape = circle 30, positionX = -300, positionY = 0, health = 100})[] [] 0 0
=======
initialState = GameState (Player {shape = circle 30, positionX = -300, positionY = 0, health = 100})[] [] 0 0 Menu {menuItem = 0}
>>>>>>> Stashed changes
