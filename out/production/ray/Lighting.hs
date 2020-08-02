module Lighting where

  import Quad
  import Colour

  data PointLight = PointLight { lightColour :: Colour, lightPosition :: Quad }

  data Material = Material { materialColour :: Colour, ambient, diffuse, specular, shininess :: Double }

  defaultMaterial :: Material
  defaultMaterial = Material white 0.1 0.9 0.9 200.0
