module Model where
import Graphics.Gloss

data Player = Player { shape     :: Shape,
                        position :: Position,
                        health   :: Health
                     }

{- data Enemy      = Shape Position Health Speed HitCheck Value
data Bullet	    = Shape Position Speed Direction Damage HitCheck Sender -}
data Direction	= Left | Right

type Shape 	    = Picture --change later to bitmap 
type Health 	= Int
type Speed 	    = Float
type Damage 	= Int
type Position   = (Int,Int)

data GameState = GameState	{ player 	:: Player,
                              {-Enemies 	:: [Enemy],
                              Bullets	:: [Bullet],
                              Time 		:: Int, -}
                              score 	:: Int
                            }


initialState :: GameState
initialState = GameState (Player {shape = circle 10, position = (0,0), health = 100}) 0