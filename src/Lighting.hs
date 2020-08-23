module Lighting where

  import Quad
  import Colour(addColour, mulScalar, mulColour)
  import ApproxEqual
  import Ray
  import Models
  import Pattern

  instance ApproxEqual PointLight
    where
      approxEqual a b =
        lightIntensity a `approxEqual` lightIntensity b &&
          lightPosition a `approxEqual` lightPosition b

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