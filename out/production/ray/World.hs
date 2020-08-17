module World where
  import Lighting
  import Ray
  import Quad
  import Colour
  import Transforms
  import Data.List

  data World = World { lights :: [PointLight], spheres :: [(ShapeId, Sphere)]} deriving (Show)

  emptyWorld :: World
  emptyWorld = World [] []

  addLight :: PointLight -> World -> World
  addLight l w =
    w {
      lights = lights w ++ [l]
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
    sortBy (\(a,b) -> t a `compare` t b ) result
    where
      result = spheres w >>= intersect r