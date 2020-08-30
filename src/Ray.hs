module Ray where

import Quad
import ApproxEqual
import Safe
import Transforms
import Models
import Colour hiding (minus)
import Data.List (delete)

data Ray = Ray { origin, direction :: Quad }

position :: Ray -> Double -> Quad
position r t =
  (direction r `scalarmul` t) `add` origin r

data Shape =
  Sphere { shapeTransform :: Matrix, shapeMaterial :: Material }
  | Plane { shapeTransform :: Matrix, shapeMaterial :: Material } deriving (Show)
      
instance ApproxEqual Shape where
  approxEqual a b =
    shapeTransform a `approxEqual` shapeTransform b &&
    shapeMaterial a `approxEqual` shapeMaterial b

plane :: Shape
plane = Plane identityM defaultMaterial

sphere :: Shape
sphere = Sphere identityM defaultMaterial

glassSphere :: Shape
glassSphere = sphere {
  shapeMaterial = defaultMaterial {
    materialTransparency = 1,
    materialRefractiveIndex = 1.5
  }
}

newtype ShapeId = ShapeId { shapeId :: Int } deriving (Show, Eq)

data Intersection = Intersection { with :: (ShapeId, Shape), t :: Double } deriving Show

instance ApproxEqual Intersection where
    approxEqual x y =
      fst (with x) == fst (with y) &&
      snd (with x) `approxEqual` snd (with y) &&
      t x `approxEqual` t y
      
instance Eq Intersection where
  (==) x y =
      fst (with x) == fst (with y) &&
      snd (with x) `approxEqual` snd (with y) &&
      t x `approxEqual` t y
        
instance Ord Intersection where
  compare x y
    | t x `approxEqual` t y = EQ
    | otherwise = compare (t x) (t y)

localIntersectAt :: (ShapeId, Shape) -> Ray -> [Intersection]
localIntersectAt shape@(_, Sphere _ _) localRay
    | discriminant < 0 = []
    | otherwise = [t1, t2]
    where
    sphereToRay = origin localRay `minus` point 0 0 0

    a = direction localRay `dot` direction localRay
    b = 2 * (direction localRay `dot` sphereToRay)
    c = (sphereToRay `dot` sphereToRay) - 1

    discriminant = (b *  b) - 4 * a * c

    t1 = Intersection shape ((-b - sqrt discriminant) / (2 * a))
    t2 = Intersection shape ((-b + sqrt discriminant) / (2 * a))

localIntersectAt shape@(_, Plane _ _) localRay
  | abs (y (direction localRay)) < epsilon = []
  | otherwise = [ Intersection shape t ]
  where
    t = -y (origin localRay) / y (direction localRay)

intersect :: Ray -> (ShapeId, Shape) -> [Intersection]
intersect ray shape =
  localIntersectAt shape localRay
  where
  localRay = ray `transform` inverse (shapeTransform (snd shape))

data Computations = Computations {
  object :: (ShapeId, Shape),
  compsT :: Double,
  compsPoint,
  compsEyeV,
  compsNormalV,
  compsReflectV,
  overPoint,
  underPoint:: Quad,
  inside :: Bool,
  n1,
  n2 :: Double
 }

computeN1N2 :: Intersection -> ((Double, Double), [(ShapeId, Shape)], Bool) -> Intersection -> ((Double, Double), [(ShapeId, Shape)], Bool)
computeN1N2 hit s@((currentN1, currentN2), containers, finished) i
  | finished = s
  | i == hit && null containers = ((1, currentRefractiveIndex), [], True)
  | i == hit && containerShapeIds == [currentShapeId] = ((currentRefractiveIndex, 1), [], True)
  | i == hit = ((lastRefractiveIndex containers, lastRefractiveIndex nextContainers), nextContainers, True)
  | otherwise = ((currentN1, currentN2), nextContainers, False)
  where
    containerShapeIds = map fst containers
    currentShapeAndId@(currentShapeId, currentShape) = with i
    currentRefractiveIndex = materialRefractiveIndex (shapeMaterial currentShape)
    lastRefractiveIndex l = materialRefractiveIndex (shapeMaterial (snd (last l)))
    nextContainers | currentShapeId `elem` containerShapeIds = filter (\(id, _) -> id /= currentShapeId) containers
                   | otherwise = containers ++ [currentShapeAndId]


prepareComputations :: Intersection -> Ray -> [Intersection] -> Computations
prepareComputations int ray allIntersections =
  Computations {
    object = obj,
    compsT = t int,
    compsPoint = p,
    compsEyeV = eyeV,
    compsNormalV = computedNormalV,
    compsReflectV = reflect (direction ray) computedNormalV,
    overPoint = p `add` (computedNormalV `scalarmul` epsilon),
    underPoint = p `minus` (computedNormalV `scalarmul` epsilon),
    inside = isInside,
    n1 = computedN1,
    n2 = computedN2
  }
  where
    obj = with int
    p = position ray (t int)
    normalV = normalAt (snd obj) p
    normalDotEye = dot normalV eyeV
    eyeV = neg (direction ray)
    isInside = normalDotEye < 0
    computedNormalV | isInside = neg normalV
                    | otherwise = normalV
    ((computedN1, computedN2), _, _) = foldl (computeN1N2 int) ((1, 1), [], False) allIntersections

hit :: [Intersection] -> Maybe Intersection
hit intersections = 
  minimumMay nonNeg
  where
    nonNeg = filter (\i -> t i >= 0) intersections

transform :: Ray -> Matrix -> Ray
transform r m =
  Ray (m `mul` origin r) (m `mul` direction r)

localNormalAt :: Shape -> Quad -> Quad
localNormalAt (Sphere _ _) localPoint =
  localPoint `minus` point 0 0 0

localNormalAt (Plane _ _) _ =
  vector 0 1 0

normalAt :: Shape -> Quad -> Quad
normalAt s worldPoint =
  normalize worldNormal'
  where
    inverseTransform = inverse (shapeTransform s)

    localPoint = inverseTransform `mul` worldPoint
    localNormal = localNormalAt s localPoint
    worldNormal = transpose inverseTransform `mul` localNormal
    worldNormal' = vector (x worldNormal) (y worldNormal) (z worldNormal)