module ApproxEqual
  where

  import Quad
  import Canvas

  class ApproxEqual a where
    approxEqual :: a -> a -> Bool

  instance ApproxEqual Double where
    approxEqual a b = ((abs (a - b)) < 0.00001)

  instance ApproxEqual Quad where
    approxEqual a b =
      approxEqual (x a) (x b) &&
      approxEqual (y a) (y b) &&
      approxEqual (z a) (z b) &&
      approxEqual (w a) (w b)

  instance ApproxEqual Colour where
    approxEqual x y =
      approxEqual (r x) (r y) &&
      approxEqual (g x) (g y) &&
      approxEqual (b x) (b y)