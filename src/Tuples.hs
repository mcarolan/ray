 module Tuples
  where

tupleX :: (Double, Double, Double, Double) -> Double
tupleX (x, _, _, _) = x

tupleY :: (Double, Double, Double, Double) -> Double
tupleY (_, y, _, _) = y

tupleZ :: (Double, Double, Double, Double) -> Double
tupleZ (_, _, z, _) = z

tupleW :: (Double, Double, Double, Double) -> Double
tupleW (_, _, _, w) = w

tupleIsPoint :: (Double, Double, Double, Double) -> Bool
tupleIsPoint t = tupleW t == 1.0

tupleIsVector :: (Double, Double, Double, Double) -> Bool
tupleIsVector t = tupleW t == 0.0

point :: Double -> Double -> Double -> (Double, Double, Double, Double)
point x y z = (x, y, z, 1.0)

vector :: Double -> Double -> Double -> (Double, Double, Double, Double)
vector x y z = (x, y, z, 0.0)
