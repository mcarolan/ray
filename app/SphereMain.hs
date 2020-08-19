module SphereMain where

import Quad
import Canvas
import Colour hiding (minus)
import Ray
import Data.Maybe
import CanvasPPM
import System.IO
import Transforms
import Lighting

colourAt :: Double -> Double -> Ray -> (ShapeId, Sphere) -> PointLight -> Colour
colourAt x y ray shape light =
  case hit (ray `intersect` shape) of
    Just h ->
      let
        p = position ray (t h)
        norm = normalAt (snd shape) p
        eye = neg (direction ray)
      in
        lighting (sphereMaterial (snd shape)) light p eye norm False
    Nothing -> black

main :: IO ()
main =
  do
    _ <- writeFile "sphere.ppm" ppm
    return ()
  where
    rayOrigin = point 0 0 (-5)
    wallZ = 10
    wallSize = 7
    canvasPixels :: Double
    canvasPixels = 500
    pixelSize = wallSize / canvasPixels
    half = wallSize / 2

    worldY y = half - pixelSize * y
    worldX x = -half + pixelSize * x
    worldPos x y = point (worldX x) (worldY y) wallZ

    rayAt x y = Ray rayOrigin (normalize (worldPos x y `minus` rayOrigin))

    pixels = [ [ colourAt x y (rayAt x y) (shapeId, shape) light | x <- [0..(canvasPixels - 1)]] | y <- [0..(canvasPixels - 1)]]

    c = listToCanvas pixels
    ppm = canvasToPPM c
    shapeId = ShapeId 0
    material = defaultMaterial { materialColour = Colour 1 0.2 1 }
    light = PointLight white (point (-10) 10 (-10))
    trans = shearing 1 0 0 0 0 0 `mul` rotateZ (pi / 4) `mul` scaling 0.5 1 1
    shape = sphere { sphereMaterial = material, sphereTransform = trans }