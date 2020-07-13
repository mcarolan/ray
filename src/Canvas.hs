module Canvas
  where

  import Data.Array
  
  data Colour = Colour { r, g, b :: Double } deriving (Show)

  add :: Colour -> Colour -> Colour
  add x y =
    Colour ((r x) + (r y)) ((g x) + (g y)) ((b x) + (b y))

  minus :: Colour -> Colour -> Colour
  minus x y =
    Colour ((r x) - (r y)) ((g x) - (g y)) ((b x) - (b y))

  mulScalar :: Colour -> Double -> Colour
  mulScalar x n =
    Colour ((r x) * n) ((g x) * n) ((b x) * n)

  mulColour :: Colour -> Colour -> Colour
  mulColour x y =
    Colour ((r x) * (r y)) ((g x) * (g y)) ((b x) * (b y))

  black = Colour 0 0 0
  white = Colour 1 1 1
  
  type Canvas = Array Int (Array Int Colour)

  buildCanvas :: Int -> Int -> Canvas
  buildCanvas w h =
    listArray (0,h-1) (replicate h row)
    where row = listArray (0,w-1) (replicate w black)
    
  writePixel :: Canvas -> Int -> Int -> Colour -> Canvas
  writePixel canvas x y colour = 
    canvas // [(y, newRow)]
    where
      row = canvas ! x
      newRow = row // [(x, colour)]
      
  pixelAt :: Canvas -> Int -> Int -> Colour
  pixelAt canvas x y =
    row ! x
    where
      row = canvas ! y