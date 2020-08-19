module World where
  import Lighting
  import Ray
  import Quad
  import Colour hiding (minus)
  import Transforms
  import Data.List(sortBy)

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

  defaultWorld :: World
  defaultWorld =
    addShape s2 (addShape s1 (addLight light emptyWorld))
    where
      light = PointLight white (point (-10) 10 (-10))
      s1 = sphere { shapeMaterial = material }
      s2 = sphere { shapeTransform = scaling 0.5 0.5 0.5 }
      material = defaultMaterial {
              materialColour = Colour 0.8 1.0 0.6,
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


  shadeHit :: World -> Computations -> Colour
  shadeHit w comps =
    lighting mat light p e n shadowed
     where
      mat = shapeMaterial (snd (object comps))
      light = head (lights w)
      p = compsPoint comps
      e = compsEyeV comps
      n = compsNormalV comps
      shadowed = isShadowed w (overPoint comps)

  colourAt :: World -> Ray -> Colour
  colourAt w r =
    case hitOpt of
      Just h ->
        shadeHit w (prepareComputations h r)
      Nothing ->
        black
    where
      intersections = intersectWorld w r
      hitOpt = hit intersections