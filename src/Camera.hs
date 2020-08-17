module Camera where

import Quad(Matrix, inverse, mul, point, normalize, minus)
import Transforms(identityM)
import Ray
import Canvas
import World

data Camera = Camera {
  hSize :: Int,
  vSize :: Int,
  fieldOfView :: Double,
  cameraTransform :: Matrix,
  inverseCameraTransform :: Matrix,
  halfWidth :: Double,
  halfHeight :: Double,
  pixelSize :: Double
 } deriving (Show)

camera :: Int -> Int -> Double -> Camera
camera hSize vSize fieldOfView =
  Camera hSize vSize fieldOfView identityM identityM halfWidth halfHeight pixelSize
  where
      halfView = tan (fieldOfView / 2)
      aspectRatio = fromIntegral hSize / fromIntegral vSize

      halfWidth | aspectRatio >= 1 = halfView
                | otherwise = halfView * aspectRatio

      halfHeight | aspectRatio >= 1 = halfView / aspectRatio
                 | otherwise = halfView

      pixelSize = halfWidth * 2 / fromIntegral hSize

setTransform :: Camera -> Matrix -> Camera
setTransform c t =
  c {
    cameraTransform = t,
    inverseCameraTransform = inverse t
  }

rayForPixel :: Int -> Int -> Camera -> Ray
rayForPixel x y c =
  Ray origin direction
  where
    xOffset = (fromIntegral x + 0.5) * pixelSize c
    yOffset = (fromIntegral y + 0.5) * pixelSize c

    worldX = halfWidth c - xOffset
    worldY = halfHeight c - yOffset

    pixel = inverseCameraTransform c `mul` point worldX worldY (-1)
    origin = inverseCameraTransform c `mul` point 0 0 0
    direction = normalize (pixel `minus` origin)

render :: Camera -> World -> Canvas
render c w =
  listToCanvas [ [ colour x y | x <- [0..(hSize c - 1)]] | y <- [0..(vSize c - 1)] ]
  where
    colour x y = colourAt w (rayForPixel x y c)