module Transforms where

  import Quad(Matrix, matrix4)

  translation :: Double -> Double -> Double -> Matrix
  translation x y z =
    matrix4   1   0   0   x
              0   1   0   y
              0   0   1   z
              0   0   0   1

  scaling :: Double -> Double -> Double -> Matrix
  scaling x y z =
    matrix4   x   0   0   0
              0   y   0   0
              0   0   z   0
              0   0   0   1