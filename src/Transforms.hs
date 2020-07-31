module Transforms where

  import Quad(Matrix, matrix4)
  
  identityM :: Matrix
  identityM =
    matrix4   1   0   0   0
              0   1   0   0
              0   0   1   0
              0   0   0   1

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

  rotateX :: Double -> Matrix
  rotateX r =
    matrix4   1           0             0                 0
              0           (cos r)       (-(sin r))        0
              0           (sin r)       (cos r)           0
              0           0             0                 1

  rotateY :: Double -> Matrix
  rotateY r =
    matrix4   (cos r)     0             (sin r)           0
              0           1             0                 0
              (-(sin r))  0             (cos r)           0
              0           0             0                 1

  rotateZ :: Double -> Matrix
  rotateZ r =
    matrix4   (cos r)     (-(sin r))    0                 0
              (sin r)     (cos r)       0                 0
              0           0             1                 0
              0           0             0                 1

  shearing :: Double -> Double -> Double -> Double -> Double -> Double -> Matrix
  shearing xy xz yx yz zx zy =
    matrix4   1   xy  xz  0
              yx  1   yz  0
              zx  zy  1   0
              0   0   0   1
