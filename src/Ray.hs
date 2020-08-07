module Ray where

import Quad
import ApproxEqual
import Safe
import Transforms
import Lighting

data Ray = Ray { origin, direction :: Quad }

position :: Ray -> Double -> Quad
position r t =
  (direction r `scalarmul` t) `add` origin r

data Sphere = Sphere { sphereTransform :: Matrix, sphereMaterial :: Material } deriving (Show)
      
instance ApproxEqual Sphere where
  approxEqual a b =
    sphereTransform a `approxEqual` sphereTransform b &&
    sphereMaterial a `approxEqual` sphereMaterial b

sphere :: Sphere
sphere = Sphere identityM defaultMaterial

newtype ShapeId = ShapeId { shapeId :: Int } deriving (Show, Eq)

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
  compare x y
    | t x `approxEqual` t y = EQ
    | otherwise = compare (t x) (t y)

intersect :: (ShapeId, Sphere) -> Ray -> [Intersection]
intersect sphere ray
  | discriminant < 0 = []
  | otherwise = [t1, t2]
  where
  ray' = ray `transform` inverse (sphereTransform (snd sphere))
  sphereToRay = origin ray' `minus` point 0 0 0

  a = direction ray' `dot` direction ray'
  b = 2 * (direction ray' `dot` sphereToRay)
  c = (sphereToRay `dot` sphereToRay) - 1

  discriminant = (b *  b) - 4 * a * c

  t1 = Intersection (fst sphere) ((-b - sqrt discriminant) / (2 * a))
  t2 = Intersection (fst sphere) ((-b + sqrt discriminant) / (2 * a))

hit :: [Intersection] -> Maybe Intersection
hit intersections = 
  minimumMay nonNeg
  where
    nonNeg = filter (\i -> t i >= 0) intersections

transform :: Ray -> Matrix -> Ray
transform r m =
  Ray (m `mul` origin r) (m `mul` direction r)

normalAt :: Sphere -> Quad -> Quad
normalAt s worldPoint =
  normalize worldNormal'
  where
    inverseTransform = inverse (sphereTransform s)
    objectPoint = inverseTransform `mul` worldPoint
    objectNormal = objectPoint `minus` point 0 0 0
    worldNormal = transpose inverseTransform `mul` objectNormal
    worldNormal' = vector (x worldNormal) (y worldNormal) (z worldNormal)