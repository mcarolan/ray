 module Quad
  where

  data Quad a = Quad { x, y, z, w :: a } deriving (Show)

  isPoint :: (Floating a, Eq a) => Quad a -> Bool
  isPoint q = (w q) == 1.0

  isVector :: (Floating a, Eq a) => Quad a -> Bool
  isVector q = (w q) == 0.0

  point :: (Floating a) => a -> a-> a -> Quad a
  point x y z = Quad x y z 1.0

  vector :: (Floating a) => a -> a -> a -> Quad a
  vector x y z = Quad x y z 0.0

  add :: (Floating a) => Quad a -> Quad a -> Quad a
  add a b = Quad x' y' z' w'
    where x' = (x a + x b)
          y' = (y a + y b)
          z' = (z a + z b)
          w' = (w a + w b)

  minus :: (Floating a) => Quad a -> Quad a -> Quad a
  minus a b = Quad x' y' z' w'
     where x' = (x a - x b)
           y' = (y a - y b)
           z' = (z a - z b)
           w' = (w a - w b)

  neg :: (Floating a) => Quad a -> Quad a
  neg = minus (Quad 0 0 0 0)

  mul :: (Floating a) => Quad a -> a -> Quad a
  mul q n = Quad (x q * n) (y q * n) (z q * n) (w q * n)

  divide :: (Floating a) => Quad a -> a -> Quad a
  divide q n = mul q (1 / n)

  magnitude :: (Floating a) => Quad a -> a
  magnitude q = sqrt (square x' + square y' + square z' + square w')
    where x' = x q
          y' = y q
          z' = z q
          w' = w q
          square n = n ^ 2

  normalize :: (Floating a) => Quad a -> Quad a
  normalize q =
    Quad ((x q) / mag) ((y q) / mag) ((z q) / mag) ((w q) / mag)
    where mag = magnitude q

  dot :: (Floating a) => Quad a -> Quad a -> a
  dot a b =
    xProd + yProd + zProd + wProd
    where xProd = (x a) * (x b)
          yProd = (y a) * (y b)
          zProd = (z a) * (z b)
          wProd = (w a) * (w b)

  cross :: (Floating a) => Quad a -> Quad a -> Quad a
  cross a b = vector x' y' z'
    where x' = ((y a) * (z b)) - ((z a) * (y b))
          y' = ((z a) * (x b)) - ((x a) * (z b))
          z' = ((x a) * (y b)) - ((y a) * (x b))

  class ApproxEqual a where
    approxEqual :: a -> a -> Bool

  instance ApproxEqual Double where
    approxEqual a b = ((abs (a - b)) < 0.00001)

  instance ApproxEqual a => ApproxEqual (Quad a) where
    approxEqual a b =
      approxEqual (x a) (x b) &&
      approxEqual (y a) (y b) &&
      approxEqual (z a) (z b) &&
      approxEqual (w a) (w b)