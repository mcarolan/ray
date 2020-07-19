 module Quad
  where

  import Data.Array

  data Quad = Quad { x, y, z, w :: Double } deriving (Show)

  quadFromMatrix :: Matrix -> Quad
  quadFromMatrix m =
    Quad (m `at` (0, 0)) (m `at` (1, 0)) (m `at` (2, 0)) (m `at` (3, 0))

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

  type Matrix = Array (Int, Int) Double

  matrixFromQuad :: Quad -> Matrix

  matrixFromQuad q =
    array ((0, 0), (4, 1)) [ ((0, 0), x q), ((1, 0), y q), ((2, 0), z q), ((3, 0), w q) ]

  matrix4 :: Double -> Double -> Double -> Double ->
             Double -> Double -> Double -> Double ->
             Double -> Double -> Double -> Double ->
             Double -> Double -> Double -> Double ->
             Matrix

  matrix4 a b c d
          e f g h
          i j k l
          m n o p =
          array ((0, 0), (4, 4)) [
            ((0, 0), a), ((0, 1), b), ((0, 2), c), ((0, 3), d),
            ((1, 0), e), ((1, 1), f), ((1, 2), g), ((1, 3), h),
            ((2, 0), i), ((2, 1), j), ((2, 2), k), ((2, 3), l),
            ((3, 0), m), ((3, 1), n), ((3, 2), o), ((3, 3), p) ]

  matrix2 :: Double -> Double ->
             Double -> Double ->
             Matrix

  matrix2 a b
          c d =
          array ((0, 0), (2, 2)) [
            ((0, 0), a), ((0, 1), b),
            ((1, 0), c), ((1, 1), d) ]

  matrix3 :: Double -> Double -> Double ->
             Double -> Double -> Double ->
             Double -> Double -> Double ->
             Matrix

  matrix3  a b c
           d e f
           g h i =
         array ((0, 0), (3, 3)) [
          ((0, 0), a), ((0, 1), b), ((0, 2), c),
          ((1, 0), d), ((1, 1), e), ((1, 2), f),
          ((2, 0), g), ((2, 1), h), ((2, 2), i) ]

  columns :: Matrix -> Int
  columns m = snd (snd (bounds m))

  rows :: Matrix -> Int
  rows m = fst (snd (bounds m))

  matmul :: Matrix -> Matrix -> Matrix
  matmul a b
    | n /= p = error ("number of columns in a did not match number of rows in b for " ++ show (rows a) ++ "x" ++ show (columns a) ++ " and " ++ show (rows b) ++ "x" ++ show (columns b) ++ " matrices")
    | otherwise = array ((0, 0), (rows a, columns b)) [ ((i, j), valueAt i j) | i <- [0..m - 1], j <- [0..q - 1] ]
    where
      productsAt i j = [ (a `at` (i, k)) * (b `at` (k, j)) | k <- [0 .. p - 1]]
      valueAt i j = sum (productsAt i j)
      m = rows a
      n = columns a
      p = rows b
      q = columns b

  mattupmul :: Matrix -> Quad -> Quad
  mattupmul a q =
    quadFromMatrix (matmul a (matrixFromQuad q))

  at :: Matrix -> (Int, Int) -> Double
  at m rc@(r, c) 
    | r >= rows m || c >= columns m = error (show rc ++ " failed bounds check on " ++ show (rows m) ++ "x" ++ show (columns m) ++ " matrix")
    | otherwise = m ! rc
