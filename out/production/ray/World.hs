module World where
  import Lighting
  import Ray

  data World = World { lights :: [PointLight], spheres :: [SphereWithId]} deriving (Show)
  
  emptyWorld :: World
  emptyWorld = World [] []
  
  addLight :: PointLight -> World -> World
  addLight l w =
    w {
      lights = lights w ++ [l]
    }
    
  addSphere :: Sphere -> World -> World
  addSphere = undefined
  
  defaultWorld :: World
  defaultWorld = undefined