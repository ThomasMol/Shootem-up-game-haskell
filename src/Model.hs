module Model where
import Graphics.Gloss

data Player = Player {  shape     :: Shape,
                        positionX :: Position,
                        positionY :: Position,
                        health    :: Health
                     }
                     
data Enemy = Enemy {  shape     :: Shape,
                      positionX :: Position,
                      positionY :: Position,
                      health    :: Health,
                      speed     :: Speed
                    }                     

{- data Enemy      = Shape Position Health Speed HitCheck Value
data Bullet	    = Shape Position Speed Direction Damage HitCheck Sender -}
data Direction	= Left | Right

type Shape 	    = Picture --change later to bitmap 
type Health 	  = Int
type Speed 	    = Float
type Damage 	  = Int
type Position   = Float

data GameState = GameState	{ player 	:: Player,
                              {-Enemies 	:: [Enemy],
                              Bullets	:: [Bullet],
                              Time 		:: Int, -}
                              score 	:: Int
                            }


initialState :: GameState
initialState = GameState (Player {shape = circle 30, positionX = -300, positionY = 0, health = 100}) 0