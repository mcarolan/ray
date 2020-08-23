module Main where

import Quad
import Canvas
import Colour hiding (minus)
import Ray
import Data.Maybe
import CanvasPPM
import System.IO
import Transforms
import World
import Camera
import Lighting
import Pattern
import Models

main :: IO ()
main =
  do
    _ <- writeFile "patterns.ppm" ppm
    return ()
  where
    light = PointLight white (point (-10) 10 (-10))
    empty = addLight light emptyWorld

    s = sphere {
      shapeMaterial = defaultMaterial {
        materialPattern = (stripePattern white black) {
          patternTransform = rotateZ (pi/4) `mul` scaling 0.2 0.2 0.2
        }
      },
      shapeTransform = translation 0 1.5 0
    }

    p = plane {
      shapeMaterial = defaultMaterial {
        materialPattern = Constant blue
      }
    }

    backWall = plane {
      shapeMaterial = defaultMaterial {
        materialPattern = Constant green
      },
      shapeTransform = translation 0 0 8 `mul` rotateX (pi/2)
    }

    shapes = [ s, p, backWall]

    world = foldl (flip addShape) empty shapes

    camTransform = viewTransform (point 0 1.5 (-5)) (point 0 1 0) (vector 0 1 0)
    cam = setTransform (camera 800 600 (pi/3))  camTransform
    canvas = render cam world
    ppm = canvasToPPM canvas