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
    _ <- writeFile "patterns-reflect-refract.ppm" ppm
    return ()
  where
    light = PointLight white (point (-10) 10 (-10))
    empty = addLight light emptyWorld

    s = sphere {
      shapeMaterial = defaultMaterial {
        materialPattern = (stripePattern white black) {
          patternTransform = rotateZ (pi/4) `mul` scaling 0.2 0.2 0.2
        },
        materialReflectivity = 0.8
      },
      shapeTransform = translation 0 1.5 0
    }

    s2 = sphere {
      shapeMaterial = defaultMaterial {
        materialPattern = (gradientPattern blue red) {
          patternTransform = rotateY (pi/2)
        }
      },
      shapeTransform = translation 2 1.5 0
    }

    s3 = sphere {
     shapeMaterial = defaultMaterial {
      materialPattern = (ringPattern red blue) {
        patternTransform = scaling 0.1 0.1 0.1
      }
     },
     shapeTransform = translation (-2) 1.5 0
    }

    s4 = sphere {
      shapeMaterial = defaultMaterial {
        materialPattern = (checkerPattern white blue) {
          patternTransform = scaling 0.3 0.3 0.3
        }
      },
      shapeTransform = translation 0 3.5 0
    }

    p = plane {
      shapeMaterial = defaultMaterial {
        materialPattern = checkerPattern white red
      }
    }

    backWall = plane {
      shapeMaterial = defaultMaterial {
        materialPattern = Constant green
      },
      shapeTransform = translation 0 0 8 `mul` rotateX (pi/2)
    }

    shapes = [ s, s2, s3, s4, p, backWall]

    world = foldl (flip addShape) empty shapes

    camTransform = viewTransform (point 0 1.5 (-10)) (point 0 1 0) (vector 0 1 0)
    cam = setTransform (camera 800 600 (pi/3))  camTransform
    canvas = render cam world
    ppm = canvasToPPM canvas