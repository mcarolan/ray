module CircleMain where

import Quad
import Canvas
import Colour(red, black)
import Ray
import Data.Maybe
import CanvasPPM
import System.IO
import Transforms

main :: IO ()
main =
  do
    _ <- writeFile "circle.ppm" ppm
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
    isHit x y = isJust (hit (shape `intersect` rayAt x y))

    colourAt x y | isHit x y = red
    colourAt x y = black

    pixels = [ [ colourAt x y | x <- [0..(canvasPixels - 1)]] | y <- [0..(canvasPixels - 1)]]

    c = listToCanvas pixels
    ppm = canvasToPPM c
    shapeId = ShapeId 0
    t = shearing 1 0 0 0 0 0 `mul` rotateZ (pi / 4) `mul` scaling 0.5 1 1
    shape = (sphere shapeId) { sphereTransform = t }