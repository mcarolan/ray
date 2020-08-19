module Main where

import Quad
import Canvas
import Colour hiding (minus)
import Ray
import Data.Maybe
import CanvasPPM
import System.IO
import Transforms
import Lighting
import World
import Camera
import Lighting

main :: IO ()
main =
  do
    _ <- writeFile "scene-shadows.ppm" ppm
    return ()
  where
    floorMaterial = defaultMaterial {
      materialColour = Colour 1 0.9 0.9,
      materialSpecular = 0
    }
    floor = sphere {
      sphereTransform = scaling 10 0.01 10,
      sphereMaterial = floorMaterial
    }

    leftWall = sphere {
       sphereTransform = translation 0 0 5 `mul`
                         rotateY (-(pi/4)) `mul`
                         rotateX (pi/2) `mul`
                         scaling 10 0.01 10,
       sphereMaterial = floorMaterial
    }

    rightWall = sphere {
      sphereTransform = translation 0 0 5 `mul`
                        rotateY (pi/4) `mul`
                        rotateX (pi/2) `mul`
                        scaling 10 0.01 10,
      sphereMaterial = floorMaterial
    }

    middle = sphere {
      sphereTransform = translation (-0.5) 1 0.5 `mul`
                        shearing 1 0 0 0 0 0,
      sphereMaterial = defaultMaterial {
        materialColour = Colour 0.1 1 0.5,
        materialDiffuse = 0.7,
        materialSpecular = 0.3
      }
    }

    right = sphere {
      sphereTransform = translation 1.5 0.5 (-0.5) `mul`
                        scaling 0.5 0.5 0.5,
      sphereMaterial = defaultMaterial {
        materialColour = Colour 0.5 1 0.1,
        materialDiffuse = 0.7,
        materialSpecular = 0.3
      }
    }

    left = sphere {
      sphereTransform = translation (-1.5) 0.33 (-0.75) `mul`
                        scaling 0.33 0.33 0.33,
      sphereMaterial = defaultMaterial {
        materialColour = Colour 1 0.8 0.1,
        materialDiffuse = 0.7,
        materialSpecular = 0.3
      }
    }

    light = PointLight white (point (-10) 10 (-10))
    empty = addLight light emptyWorld

    spheres = [ floor, leftWall, rightWall, middle, right, left ]

    world = foldl (flip addSphere) empty spheres

    camTransform = viewTransform (point 0 1.5 (-5)) (point 0 1 0) (vector 0 1 0)
    cam = setTransform (camera 800 600 (pi/3))  camTransform
    canvas = render cam world
    ppm = canvasToPPM canvas