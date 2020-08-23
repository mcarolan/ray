module ClockMain where

import Canvas
import CanvasPPM
import System.IO
import Colour
import Transforms
import Quad
import Models

draw :: Quad -> Colour -> Canvas -> Canvas
draw q =
  writePixel (round(188 * x q) + 250) (round(188 * y q) + 250)

drawall:: [(Quad, Colour)] -> Canvas -> Canvas
drawall quads c =
  foldl (\canvas (quad, colour) -> draw quad colour canvas) c quads

main :: IO ()
main =
  do
    let toPlot = take 12 hours `zip` repeat green
    let final = drawall toPlot canvas
    let ppm = canvasToPPM final
    _ <- writeFile "clock.ppm" ppm
    return ()
  where
    baseCanvas = buildCanvas 500 500
    canvas = writePixel 250 250 blue baseCanvas
    nextHour p = rotateZ (pi / 6.0) `mul` p
    center = translation 250 250 0
    twelve = point 0 1 0
    hours = iterate nextHour twelve
