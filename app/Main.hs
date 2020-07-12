module Main where

import Quad
import Data.Functor
import Control.Monad.Loops

data Projectile = Projectile { position :: Quad Double, velocity :: Quad Double } deriving (Show)
data Environment = Environment { gravity :: Quad Double, wind :: Quad Double }

tick :: Environment -> Projectile -> Projectile
tick e p =
  Projectile pos vel
  where
    pos = (position p) `add` (velocity p)
    vel = ((velocity p) `add` (gravity e)) `add` (wind e)

tickAndPrint :: Environment -> Projectile -> IO Projectile
tickAndPrint e p =
  do
    _ <- print next
    return next
  where next = tick e p

tickUntilYBelow :: Environment -> Projectile -> Double -> IO Projectile
tickUntilYBelow env initialProj target =
  iterateUntilM yBelow (tickAndPrint env) initialProj
  where
    yBelow p = (y (position p)) < target

main :: IO ()
main =
  do
  final <- tickUntilYBelow env proj 0
  _ <- print ("Final projectile: " ++ show final)
  return ()
  where
    env = Environment (vector 0 (-0.1) 0) (vector (-0.01) 0 0)
    proj = Projectile (point 0 1 0) (normalize (vector 1 (-0.1) 0))
