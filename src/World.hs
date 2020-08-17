module World where
  import Lighting
  import Ray
  import Quad
  import Colour
  import Transforms
  import Data.List(sortBy)

  data World = World { lights :: [PointLight], spheres :: [(ShapeId, Sphere)]} deriving (Show)

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

  addSphere :: Sphere -> World -> World
  addSphere s w = 
    w {
      spheres = current ++ [(nextId, s)]
    }
    where
      current = spheres w
      nextId = ShapeId (length current)

  defaultWorld :: World
  defaultWorld =
    addSphere s2 (addSphere s1 (addLight light emptyWorld))
    where
      light = PointLight white (point (-10) 10 (-10))
      s1 = sphere { sphereMaterial = material }
      s2 = sphere { sphereTransform = scaling 0.5 0.5 0.5 }
      material = defaultMaterial {
              materialColour = Colour 0.8 1.0 0.6,
              materialDiffuse = 0.7,
              materialSpecular = 0.2
            }

  intersectWorld :: World -> Ray -> [Intersection]
  intersectWorld w r =
    sortBy (\a b -> t a `compare` t b) result
    where
      result = spheres w >>= intersect r

  shadeHit :: World -> Computations -> Colour
  shadeHit w comps =
    lighting mat light p e n
     where
      mat = sphereMaterial (snd (object comps))
      light = head (lights w)
      p = compsPoint comps
      e = compsEyeV comps
      n = compsNormalV comps

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