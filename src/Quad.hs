 module Quad
  where

  data Quad = Quad { x, y, z, w :: Double } deriving (Show)

  isPoint :: Quad -> Bool
  isPoint q = (w q) == 1.0

  isVector :: Quad -> Bool
  isVector q = (w q) == 0.0

  point :: Double -> Double -> Double -> Quad
  point x y z = Quad x y z 1.0

  vector :: Double -> Double -> Double -> Quad
  vector x y z = Quad x y z 0.0

  add :: Quad -> Quad -> Quad
  add a b = Quad x' y' z' w'
    where x' = (x a + x b)
          y' = (y a + y b)
          z' = (z a + z b)
          w' = (w a + w b)

  minus :: Quad -> Quad -> Quad
  minus a b = Quad x' y' z' w'
     where x' = (x a - x b)
           y' = (y a - y b)
           z' = (z a - z b)
           w' = (w a - w b)

  neg :: Quad -> Quad
  neg = minus (Quad 0 0 0 0)

  mul :: Quad -> Double -> Quad
  mul q n = Quad (x q * n) (y q * n) (z q * n) (w q * n)

  divide :: Quad -> Double -> Quad
  divide q n = mul q (1 / n)

  magnitude :: Quad -> Double
  magnitude q = sqrt (square x' + square y' + square z' + square w')
    where x' = x q
          y' = y q
          z' = z q
          w' = w q
          square n = n ^ 2

  normalize :: Quad -> Quad
  normalize q =
    Quad ((x q) / mag) ((y q) / mag) ((z q) / mag) ((w q) / mag)
    where mag = magnitude q

  dot :: Quad -> Quad -> Double
  dot a b =
    xProd + yProd + zProd + wProd
    where xProd = (x a) * (x b)
          yProd = (y a) * (y b)
          zProd = (z a) * (z b)
          wProd = (w a) * (w b)

  cross :: Quad -> Quad -> Quad
  cross a b = vector x' y' z'
    where x' = ((y a) * (z b)) - ((z a) * (y b))
          y' = ((z a) * (x b)) - ((x a) * (z b))
          z' = ((x a) * (y b)) - ((y a) * (x b))
