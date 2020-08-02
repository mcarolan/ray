 module Quad
  where

  import Data.Array(Array, array, bounds, (!))
  import Data.List(intercalate)
  import ApproxEqual

  data Quad = Quad { x, y, z, w :: Double } deriving (Show)

  instance ApproxEqual Quad where
    approxEqual a b =
      approxEqual (x a) (x b) &&
      approxEqual (y a) (y b) &&
      approxEqual (z a) (z b) &&
      approxEqual (w a) (w b)

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

  scalarmul :: Quad -> Double -> Quad
  scalarmul q n = Quad (x q * n) (y q * n) (z q * n) (w q * n)

  divide :: Quad -> Double -> Quad
  divide q n = scalarmul q (1 / n)

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
  
  class MatrixMultiply a where
    mul :: Matrix -> a -> a
    
  instance MatrixMultiply Matrix where
    mul a b 
      | n /= p = error ("number of columns in a did not match number of rows in b for:\n" ++ show a ++ "\nand\n" ++ show b)
      | otherwise = matrix (rows a) (columns b) [ valueAt r c | r <- [0..m - 1], c <- [0..q - 1] ]
      where
        productsAt r c = [ (a `at` (r, k)) * (b `at` (k, c)) | k <- [0 .. p - 1]]
        valueAt r c = sum (productsAt r c)
        m = rows a
        n = columns a
        p = rows b
        q = columns b
        
  instance MatrixMultiply Quad where
    mul a q =
      quadFromMatrix (a `mul` matrixFromQuad q)

  instance Show Matrix where
    show m =
      show (rows m) ++ "x" ++ show (columns m) ++ " matrix:\n" ++
      numbers
      where
        rowNumbers r = intercalate "\t|\t" [ show (m `at` (r, c)) | c <- [0..columns m - 1]]
        allRows = [ rowNumbers r | r <- [0..rows m - 1]]
        numbers = unlines allRows

  instance ApproxEqual Matrix where
    approxEqual a b =
      columns a == columns b &&
      rows a == rows b &&
      all (uncurry approxEqual) [ (a `at` (i, j), b `at` (i, j)) | i <- [0..rows a - 1], j <- [0..columns a - 1]]

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

  transpose :: Matrix -> Matrix
  transpose m =
    matrix (rows m) (columns m) [ m `at` (c, r) | r <- [0..rows m - 1], c <- [0..columns m - 1]]

  determinant :: Matrix -> Double
  determinant m
    | rows m == 2 && columns m == 2 =
      (m `at` (0, 0)) * (m `at` (1, 1)) - (m `at` (0, 1)) * (m `at` (1, 0))
    | otherwise =
      sum [valueAt c | c <- [0..columns m - 1]]
    where
      valueAt c = m `at` (0, c) * cofactor 0 c m

  submatrix :: Int -> Int -> Matrix -> Matrix
  submatrix r c m =
    matrix (rows m - 1) (columns m - 1) [ m `at` (i, j) | i <- selectedRows, j <- selectedCols ]
    where
      selectedRows = [i | i <- [0..rows m - 1], i /= r ]
      selectedCols = [i | i <- [0..columns m - 1], i /= c]

  minor :: Int -> Int -> Matrix -> Double
  minor r c m =
    determinant (submatrix r c m)

  cofactor :: Int -> Int -> Matrix -> Double
  cofactor r c m =
    negate * minor r c m
    where
      negate | even (r + c) = 1
             | otherwise = -1

  invertable :: Matrix -> Bool
  invertable m = not (determinant m `approxEqual` 0)

  inverse :: Matrix -> Matrix
  inverse m
    | det `approxEqual` 0 = error("cannot invert as determinant is 0 for " ++ show m)
    | otherwise =
      transpose m'
    where
      m' = matrix (rows m) (columns m) els
      det = determinant m
      valueAt r c = cofactor r c m / det
      els = [ valueAt r c | r <- [0..rows m - 1], c <- [0..columns m - 1]]

  at :: Matrix -> (Int, Int) -> Double
  at m rc@(r, c)
    | r >= rows m || c >= columns m = error (show rc ++ " failed bounds check on " ++ show m)
    | otherwise = elems m ! rc

  reflect :: Quad -> Quad -> Quad
  reflect v normal =
    v `minus` (normal `scalarmul` 2 `scalarmul` dot v normal)