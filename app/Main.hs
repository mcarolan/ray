module Main where

import Quad
import Data.Functor
import Control.Monad.Loops
import Canvas
import Colour
import CanvasPPM
import System.IO

data Projectile = Projectile { position :: Quad, velocity :: Quad } deriving (Show)
data Environment = Environment { gravity :: Quad, wind :: Quad }

tick :: Environment -> Projectile -> Projectile
tick e p =
  Projectile pos vel
  where
    pos = (position p) `add` (velocity p)
    vel = ((velocity p) `add` (gravity e)) `add` (wind e)

plot :: Projectile -> Canvas -> Canvas
plot projectile canvas =
  writePixel canvasX canvasY red canvas
  where
    canvasX = round (x (position projectile))
    canvasY = height canvas - round (y (position projectile))

tickAndPlot :: Environment -> (Projectile, Canvas) -> (Projectile, Canvas)
tickAndPlot env (proj, canv) =
  (next, nextCanvas)
  where
    next = tick env proj
    nextCanvas = plot next canv

tickWhileAbove :: Environment -> (Projectile, Canvas) -> Canvas
tickWhileAbove env (proj, canvas)
  | yAbove0 nextProjectile = tickWhileAbove env next
  | otherwise = canvas
  where
    next@(nextProjectile, _) = tickAndPlot env (proj, canvas)
    yAbove0 p = (y (position p)) > 0

main :: IO ()
main =
  do
    let final = tickWhileAbove environment (projectile, canvas)
    let ppm = canvasToPPM final
    _ <- writeFile "output.ppm" ppm
    return ()
  where
    gravity = vector 0 (-0.1) 0
    wind = vector (-0.01) 0 0
    environment = Environment gravity wind

    start = point 0 1 0
    velocity = (normalize (vector 1 1.8 0)) `scalarmul` 11.25
    projectile = Projectile start velocity

    canvas = buildCanvas 900 550