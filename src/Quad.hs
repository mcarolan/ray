 module Quad
  where

  import Data.Array(Array, array, bounds, (!))
  import Data.List(intercalate)

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

  newtype Matrix = Matrix { elems :: Array (Int, Int) Double}

  instance Show Matrix where
    show m =
      show (rows m) ++ "x" ++ show (columns m) ++ " matrix:\n" ++
      numbers
      where
        rowNumbers r = intercalate "\t|\t" [ show (m `at` (r, c)) | c <- [0..columns m - 1]]
        allRows = [ rowNumbers r | r <- [0..rows m - 1]]
        numbers = unlines allRows

  matrix :: Int -> Int -> [Double] -> Matrix
  matrix rows cols elems
    | length elems /= rows * cols = error (show (length elems) ++ " is the wrong number of elems for a " ++ show rows ++ "x" ++ show cols ++ " matrix")
    | otherwise = Matrix (array ((0, 0), (rows, cols)) (indices `zip` elems))
    where
      indices = [ (r, c) | r <- [0..rows - 1], c <- [0..cols - 1]]

  matrixFromQuad :: Quad -> Matrix
  matrixFromQuad q =
    matrix 4 1 [ x q, y q, z q, w q ]

  matrix4 :: Double -> Double -> Double -> Double ->
             Double -> Double -> Double -> Double ->
             Double -> Double -> Double -> Double ->
             Double -> Double -> Double -> Double ->
             Matrix

  matrix4 a b c d
          e f g h
          i j k l
          m n o p =
          matrix 4 4 [
            a, b, c, d,
            e, f, g, h,
            i, j, k, l,
            m, n, o, p
          ]

  matrix2 :: Double -> Double ->
             Double -> Double ->
             Matrix

  matrix2 a b
          c d =
          matrix 2 2 [
            a, b,
            c, d
          ]

  matrix3 :: Double -> Double -> Double ->
             Double -> Double -> Double ->
             Double -> Double -> Double ->
             Matrix

  matrix3  a b c
           d e f
           g h i =
           matrix 3 3 [
            a, b, c,
            d, e, f,
            g, h, i
           ]

  columns :: Matrix -> Int
  columns m = snd (snd (bounds (elems m)))

  rows :: Matrix -> Int
  rows m = fst (snd (bounds (elems m)))

  identity4 :: Matrix
  identity4 =
    matrix4 1 0 0 0
            0 1 0 0
            0 0 1 0
            0 0 0 1

  matmul :: Matrix -> Matrix -> Matrix
  matmul a b
    | n /= p = error ("number of columns in a did not match number of rows in b for:\n" ++ show a ++ "\nand\n" ++ show b)
    | otherwise = matrix (rows a) (columns b) [ valueAt r c | r <- [0..m - 1], c <- [0..q - 1] ]
    where
      productsAt r c = [ (a `at` (r, k)) * (b `at` (k, c)) | k <- [0 .. p - 1]]
      valueAt r c = sum (productsAt r c)
      m = rows a
      n = columns a
      p = rows b
      q = columns b

  mattupmul :: Matrix -> Quad -> Quad
  mattupmul a q =
    quadFromMatrix (matmul a (matrixFromQuad q))

  at :: Matrix -> (Int, Int) -> Double
  at m rc@(r, c)
    | r >= rows m || c >= columns m = error (show rc ++ " failed bounds check on " ++ show m)
    | otherwise = elems m ! rc
