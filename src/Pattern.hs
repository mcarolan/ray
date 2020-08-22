module Pattern where
import Colour
import Quad
import ApproxEqual
import Transforms
import Ray

type StripePattern = (Colour, Colour)

data Pattern = StripePattern { a, b :: Colour, patternTransform :: Matrix }
               | Constant { c :: Colour } deriving (Show)

stripePattern :: Colour -> Colour -> Pattern
stripePattern a b = StripePattern a b identityM

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