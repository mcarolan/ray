module Lighting where

  import Quad
  import Colour(Colour, addColour, mulScalar, mulColour, black, white)
  import ApproxEqual
  import Ray

  data PointLight = PointLight { lightIntensity :: Colour, lightPosition :: Quad } deriving (Show)

  instance ApproxEqual PointLight
    where
      approxEqual a b =
        lightIntensity a `approxEqual` lightIntensity b &&
          lightPosition a `approxEqual` lightPosition b

  data Material = Material { 
  materialAmbient, 
  materialDiffuse, 
  materialSpecular, 
  materialShininess :: Double,
  materialPattern :: Pattern } deriving (Show)

  instance ApproxEqual Material
    where
      approxEqual a b =
        materialPattern a `approxEqual` materialPattern b &&
        materialAmbient a `approxEqual` materialAmbient b &&
        materialDiffuse a `approxEqual` materialDiffuse b &&
        materialSpecular a `approxEqual` materialSpecular b &&
        materialShininess a `approxEqual` materialShininess b

  defaultMaterial :: Material
  defaultMaterial = Material 0.1 0.9 0.9 200.0 (Constant white)

  lighting :: Material -> Shape -> PointLight -> Quad -> Quad -> Quad -> Bool -> Colour
  lighting m shape light pos eye norm inShadow
    | inShadow = ambient
    | otherwise = ambient `addColour` diffuse `addColour` specular
    where
      li = lightIntensity light

      colour = patternColourForObject (materialPattern m) shape pos
      effectiveColour = colour `mulColour` li

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