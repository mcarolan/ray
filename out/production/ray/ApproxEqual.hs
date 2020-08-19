{-# LANGUAGE FlexibleInstances #-}
module ApproxEqual
  where

  import Colour
  import Data.Array
  import Data.Maybe

  epsilon = 0.00001
  
  class ApproxEqual a where
    approxEqual :: a -> a -> Bool

  instance ApproxEqual Double where
    approxEqual a b = ((abs (a - b)) < epsilon)

  instance ApproxEqual Colour where
    approxEqual x y =
      approxEqual (r x) (r y) &&
      approxEqual (g x) (g y) &&
      approxEqual (b x) (b y)

  instance (ApproxEqual a) => ApproxEqual [a] where
    approxEqual x y =
      length x == length y &&
      all (uncurry approxEqual) (zip x y)

  instance (ApproxEqual a) => ApproxEqual (Maybe a) where
    approxEqual x y =
      case (x, y) of
        (Just xValue, Just yValue) -> xValue `approxEqual` yValue
        (Nothing, Nothing) -> True
        _ -> False