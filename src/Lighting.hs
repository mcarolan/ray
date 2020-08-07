module Lighting where

  import Quad
  import Colour(Colour, addColour, mulScalar, mulColour, black, white)
  import ApproxEqual

  data PointLight = PointLight { lightIntensity :: Colour, lightPosition :: Quad } deriving (Show)

  instance ApproxEqual PointLight
    where
      approxEqual a b =
        lightIntensity a `approxEqual` lightIntensity b &&
          lightPosition a `approxEqual` lightPosition b

  data Material = Material { materialColour :: Colour, materialAmbient, materialDiffuse, materialSpecular, materialShininess :: Double } deriving (Show)

  instance ApproxEqual Material
    where
      approxEqual a b =
        materialColour a `approxEqual` materialColour b &&
        materialAmbient a `approxEqual` materialAmbient b &&
        materialDiffuse a `approxEqual` materialDiffuse b &&
        materialSpecular a `approxEqual` materialSpecular b &&
        materialShininess a `approxEqual` materialShininess b

  defaultMaterial :: Material
  defaultMaterial = Material white 0.1 0.9 0.9 200.0

  lighting :: Material -> PointLight -> Quad -> Quad -> Quad -> Colour
  lighting m light pos eye norm =
    ambient `addColour` diffuse `addColour` specular
    where
      li = lightIntensity light

      effectiveColour = materialColour m `mulColour` li

      ambient = effectiveColour `mulScalar` materialAmbient m

      lightv = normalize (lightPosition light `minus` pos)
      lightDotNormal = lightv `dot` norm

      diffuse
        | lightDotNormal < 0 = black
        | otherwise = effectiveColour `mulScalar` materialDiffuse m `mulScalar` lightDotNormal

      reflectv = reflect (neg lightv) norm
      reflectDotEye = reflectv `dot` eye
      factor = reflectDotEye ** materialShininess m

      specular
        | lightDotNormal < 0 = black
        | reflectDotEye <= 0 = black
        | otherwise = li `mulScalar` materialSpecular m `mulScalar` factor