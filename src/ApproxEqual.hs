{-# LANGUAGE FlexibleInstances #-}
module ApproxEqual
  where

  import Colour
  import Data.Array
  import Data.Maybe
  import Models

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
  
  instance ApproxEqual Material
    where
      approxEqual a b =
        materialPattern a `approxEqual` materialPattern b &&
        materialAmbient a `approxEqual` materialAmbient b &&
        materialDiffuse a `approxEqual` materialDiffuse b &&
        materialSpecular a `approxEqual` materialSpecular b &&
        materialShininess a `approxEqual` materialShininess b
        
  instance ApproxEqual Matrix where
    approxEqual a b =
      columns a == columns b &&
      rows a == rows b &&
      all (uncurry approxEqual) [ (a `at` (i, j), b `at` (i, j)) | i <- [0..rows a - 1], j <- [0..columns a - 1]]
  
  instance ApproxEqual Pattern where
    approxEqual a b =
      case (a, b) of
        (Constant c1, Constant c2) ->
          c1 `approxEqual` c2
        (StripePattern a b m, StripePattern a' b' m') ->
          a `approxEqual` a' &&
          b `approxEqual` b' &&
          m `approxEqual` m'
        _ ->
          False

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