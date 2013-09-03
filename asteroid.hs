
module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Interface.Pure.Simulate
import Graphics.Gloss.Interface.Pure.Display

data AsteroidWorld = Play [Rock] Ship [Bullet]
                   | GameOver 
                   deriving (Eq,Show)


type PointInSpace = (Float, Float)
type Velocity     = (Float, Float)
type Size         = Float
type Age          = Float


data Ship   = Ship   PointInSpace Velocity      deriving (Eq,Show)
data Bullet = Bullet PointInSpace Velocity Age  deriving (Eq,Show)
data Rock   = Rock   PointInSpace Size Velocity deriving (Eq,Show)



initialWorld :: AsteroidWorld
initialWorld = Play
                   [Rock (150,150)  45 (2,6)    -- The default rocks
                   ,Rock (-45,201)  45 (13,-8) 
                   ,Rock (45,22)    25 (-2,8)  
                   ,Rock (-210,-15) 30 (-2,-8) 
                   ,Rock (-45,-201) 25 (8,2)   
                   ]
                   (Ship (0,0) (0,5))           -- The initial ship
                   []                           -- The initial bullets (none)


simulateWorld :: Float -> (AsteroidWorld -> AsteroidWorld)

simulateWorld _        GameOver          = GameOver  

simulateWorld timeStep (Play rocks (Ship shipPos shipV) bullets) 
  | any (collidesWith shipPos) rocks = GameOver
  | otherwise = Play (concatMap updateRock rocks) 
                              (Ship newShipPos shipV)
                              (concat . map updateBullet $ bullets)
  where
      collidesWith :: PointInSpace -> Rock -> Bool
      collidesWith p (Rock rp s _) 
       = magV (rp .- p) < s 

      collidesWithBullet :: Rock -> Bool
      collidesWithBullet r 
       = any (\(Bullet bp _ _) -> collidesWith bp r) bullets 
     
      updateRock :: Rock -> [Rock]
      updateRock r@(Rock p s v) 
       | collidesWithBullet r && s < 7 
            = []
       | collidesWithBullet r && s > 7 
            = splitRock r
       | otherwise                     
            = [Rock (restoreToScreen $ p .+ timeStep .* v) s v]
 
      updateBullet :: Bullet -> [Bullet] 
      updateBullet (Bullet p v a) 
        | a > 5                      
             = []
        | any (collidesWith p) rocks 
             = [] 
        | otherwise                  
             = [Bullet (restoreToScreen $ p .+ timeStep .* v) v 
                       (a + timeStep)] 

      newShipPos :: PointInSpace
      newShipPos = restoreToScreen $ shipPos .+ timeStep .* shipV



splitRock :: Rock -> [Rock]
splitRock (Rock p s v) = [Rock p (s/2) (3 .* rotateV (pi/3)  v)
                         ,Rock p (s/2) (3 .* rotateV (-pi/3) v) ]

restoreToScreen :: PointInSpace -> PointInSpace
restoreToScreen (x,y) = (cycleCoordinates x, cycleCoordinates y)

cycleCoordinates :: (Ord a, Num a) => a -> a
cycleCoordinates x 
    | x < (-400) = 800+x
    | x > 400    = x-800
    | otherwise  = x



(.-) , (.+) :: PointInSpace -> PointInSpace -> PointInSpace
(x,y) .- (u,v) = (x-u,y-v)
(x,y) .+ (u,v) = (x+u,y+v)
infixl 6 .- , .+
infixl 7 .*

(.*) :: Float -> PointInSpace -> PointInSpace
s .* (u,v) = (s*u,s*v)

norm :: PointInSpace -> PointInSpace
norm (x,y) = let m = magV (x,y) in (x/m,y/m)

magV :: PointInSpace -> Float
magV (x,y) = sqrt (x**2 + y**2) 

rotateV :: Float -> PointInSpace -> PointInSpace
rotateV r (x,y) = (x * cos r - y * sin r
                  ,x * sin r + y * cos r)




drawWorld :: AsteroidWorld -> Picture

drawWorld GameOver 
   = scale 0.3 0.3 
     . translate (-400) 0 
     . color red 
     . text 
     $ "Game Over!"

drawWorld (Play rocks (Ship (x,y) (vx,vy)) bullets)
   = pictures [ship, asteroids,shots]
    where 
     ship      = color red . pictures $ [translate x y . circle $ 10]
     asteroids = pictures [translate x y . color orange . circle $ s 
                          | Rock   (x,y) s _ <- rocks]
     shots     = pictures [translate x y . color red . circle    $ 2 
                          | Bullet (x,y) _ _ <- bullets]






handleEvents :: Event -> AsteroidWorld -> AsteroidWorld

handleEvents _ GameOver = GameOver

handleEvents (EventKey (MouseButton LeftButton) Down _ clickPos)
             (Play rocks (Ship shipPos shipVel) bullets)
             = Play rocks (Ship shipPos newVel) (newBullet : bullets)
 where 
     newBullet = Bullet shipPos (-150 .* norm (shipPos .- clickPos)) 0
     newVel    = shipVel .+ (50 .* norm (shipPos .-  clickPos))

handleEvents _ w = w



main = play 
         (InWindow "Asteroids!" (550,550) (20,20)) 
         black 
         24 
         initialWorld 
         drawWorld 
               handleEvents
         simulateWorld

