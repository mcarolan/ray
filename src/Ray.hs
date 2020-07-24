module Ray where

import Quad
import ApproxEqual
import Safe

data Ray = Ray { origin, direction :: Quad }

position :: Ray -> Double -> Quad
position r t =
  (direction r `scalarmul` t) `add` origin r

newtype Sphere = Sphere { sphereId :: ShapeId }

data ShapeId = ShapeId { shapeId :: Int } deriving (Show, Eq)

data Intersection = Intersection { with :: ShapeId, t :: Double } deriving Show

instance ApproxEqual Intersection where
    approxEqual x y =
      with x == with y &&
      t x `approxEqual` t y
      
instance Eq Intersection where
  (==) x y =
    with x == with y &&
      t x `approxEqual` t y
      
instance Ord Intersection where
  compare x y =
    compare (t x) (t y)

intersect :: Sphere -> Ray -> [Intersection]
intersect sphere ray
  | discriminant < 0 = []
  | otherwise = [t1, t2]
  where
  sphereToRay = origin ray `minus` point 0 0 0

  a = direction ray `dot` direction ray
  b = 2 * (direction ray `dot` sphereToRay)
  c = (sphereToRay `dot` sphereToRay) - 1

  discriminant = (b *  b) - 4 * a * c

  t1 = Intersection (sphereId sphere) ((-b - sqrt discriminant) / (2 * a))
  t2 = Intersection (sphereId sphere) ((-b + sqrt discriminant) / (2 * a))

hit :: [Intersection] -> Maybe Intersection
hit intersections = 
  minimumMay nonNeg
  where
    nonNeg = filter (\i -> t i >= 0) intersections
  