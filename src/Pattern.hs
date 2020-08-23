module Pattern where
import Colour
import Quad
import ApproxEqual
import Transforms
import Ray
import Models

stripePattern :: Colour -> Colour -> Pattern
stripePattern a b = StripePattern a b identityM

patternColourAt :: Pattern -> Quad -> Colour
patternColourAt (StripePattern a b _) p
  | even (floor (x p)) = a
  | otherwise = b

patternColourAt (Constant c) _ = c

patternColourForObject :: Pattern -> Shape -> Quad -> Colour
patternColourForObject pat shape point =
  patternColourAt pat patternPoint
  where
    objectPoint = inverse (shapeTransform shape) `mul` point
    patternPoint = inverse (patternTransform pat) `mul` objectPoint