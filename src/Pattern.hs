module Pattern where
import Colour
import Quad hiding (minus)
import ApproxEqual
import Transforms
import Ray
import Models

prettyMuchFloor :: Double -> Int
prettyMuchFloor d 
  | d `approxEqual` fromIntegral f1 = f1
  | otherwise = f
  where
    f = floor d
    f1 = f + 1
    
stripePattern :: Colour -> Colour -> Pattern
stripePattern a b = StripePattern a b identityM

gradientPattern :: Colour -> Colour -> Pattern
gradientPattern from to = GradientPattern from to identityM

ringPattern :: Colour -> Colour -> Pattern
ringPattern on off = RingPattern on off identityM

checkerPattern :: Colour -> Colour -> Pattern
checkerPattern on off = CheckerPattern on off identityM

patternColourAt :: Pattern -> Quad -> Colour
patternColourAt (StripePattern a b _) p
  | even (prettyMuchFloor (x p)) = a
  | otherwise = b

patternColourAt (GradientPattern from to _) p =
  from `addColour` (distance `mulScalar` fraction)
  where
    distance = to `minus` from
    floorX = fromIntegral (prettyMuchFloor (x p))
    fraction = x p -  floorX

patternColourAt (RingPattern on off _) p
  | even n = on
  | otherwise = off
  where
    xSquared = x p ^ 2
    zSquared = z p ^ 2
    n = prettyMuchFloor (sqrt (xSquared + zSquared))

patternColourAt (CheckerPattern on off _) p
  | even n = on
  | otherwise = off
  where
    n :: Int
    n = prettyMuchFloor (x p) + prettyMuchFloor (y p) + prettyMuchFloor (z p)

patternColourAt (Constant c) _ = c

patternColourForObject :: Pattern -> Shape -> Quad -> Colour
patternColourForObject pat shape point =
  patternColourAt pat patternPoint
  where
    objectPoint = inverse (shapeTransform shape) `mul` point
    patternPoint = inverse (patternTransform pat) `mul` objectPoint