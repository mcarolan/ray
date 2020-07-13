module Canvas
  where
  import Data.Array.IArray
  
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
  
  type Canvas = IArray (IArray Colour Int) Int

  buildCanvas :: Int -> Int -> Canvas
  buildCanvas w h =
    replicate h row
    where row = replicate w black
    
  writePixel :: Canvas -> Int -> Int -> Colour -> Canvas
  writePixel canvas x y colour = 
    undefined
    where 
      row = canvas !! y
      newRow = (take x row) 