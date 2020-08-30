module World where
  import Lighting
  import Ray
  import Quad
  import Colour hiding (minus)
  import Transforms
  import Data.List(sortBy)
  import Pattern
  import Models
  import ApproxEqual

  data World = World { lights :: [PointLight], shapes :: [(ShapeId, Shape)]} deriving (Show)

  emptyWorld :: World
  emptyWorld = World [] []

  addLight :: PointLight -> World -> World
  addLight l w =
    w {
      lights = lights w ++ [l]
    }

  setLight :: PointLight -> World -> World
  setLight l w =
    w {
      lights = [l]
    }

  addShape :: Shape -> World -> World
  addShape s w = 
    w {
      shapes = current ++ [(nextId, s)]
    }
    where
      current = shapes w
      nextId = ShapeId (length current)

  defaultLight :: PointLight
  defaultLight = PointLight white (point (-10) 10 (-10))

  defaultWorld :: World
  defaultWorld =
    addShape s2 (addShape s1 (addLight defaultLight emptyWorld))
    where
      s1 = sphere { shapeMaterial = material }
      s2 = sphere { shapeTransform = scaling 0.5 0.5 0.5 }
      material = defaultMaterial {
              materialPattern = Constant (Colour 0.8 1.0 0.6),
              materialDiffuse = 0.7,
              materialSpecular = 0.2
            }

  intersectWorld :: World -> Ray -> [Intersection]
  intersectWorld w r =
    sortBy (\a b -> t a `compare` t b) result
    where
      result = shapes w >>= intersect r

  isShadowed :: World -> Quad -> Bool
  isShadowed w p =
    case hOpt of
      Just h ->
        t h < distance
      Nothing ->
        False
    where
      v = lightPosition (head (lights w)) `minus` p
      distance = magnitude v
      direction = normalize v

      r = Ray p direction
      intersections = intersectWorld w r
      hOpt = hit intersections


  shadeHit :: World -> Computations -> Int -> Colour
  shadeHit w comps remaining =
    surface `addColour` reflected
     where
      shape = snd (object comps)
      mat = shapeMaterial shape
      light = head (lights w)
      p = compsPoint comps
      e = compsEyeV comps
      n = compsNormalV comps
      shadowed = isShadowed w (overPoint comps)
      surface = lighting mat shape light p e n shadowed
      reflected = reflectedColour w comps remaining

  colourAt :: World -> Ray -> Colour
  colourAt w r = colourAtWithRemaining w r 5
  
  colourAtWithRemaining :: World -> Ray -> Int -> Colour
  colourAtWithRemaining w r remaining =
    case hitOpt of
      Just h ->
        shadeHit w (prepareComputations h r []) remaining
      Nothing ->
        black
    where
      intersections = intersectWorld w r
      hitOpt = hit intersections

  reflectedColour :: World -> Computations -> Int -> Colour
  reflectedColour world comps remaining
    | remaining == 0 = black
    | reflectivity `approxEqual` 0 = black
    | otherwise = colour `mulScalar` reflectivity
    where
      reflectivity = materialReflectivity (shapeMaterial (snd (object comps)))
      colour = colourAtWithRemaining world reflectRay (remaining - 1)
      reflectRay = Ray (overPoint comps) (compsReflectV comps)

  refractedColour :: World -> Computations -> Int -> Colour
  refractedColour world comps remaining
    | remaining == 0 = black
    | materialTransparency (shapeMaterial (snd (object comps))) `approxEqual` 0 = black
    | hasTotalInternalReflection = black
    | otherwise = white
    where
      nRatio = n1 comps / n2 comps
      cosI = compsEyeV comps `dot` compsNormalV comps
      sin2T = nRatio^2 * (1 - cosI^2)
      hasTotalInternalReflection = not (sin2T `approxEqual` 1) && sin2T > 1