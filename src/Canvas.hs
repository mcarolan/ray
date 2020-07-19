module Canvas
  where

  import Data.Array
  import Data.List
  import Colour
  
  type Canvas = Array Int (Array Int Colour)

  buildCanvas :: Int -> Int -> Canvas
  buildCanvas w h =
    listArray (0,h-1) (replicate h row)
    where row = listArray (0,w-1) (replicate w black)

  writePixel :: Int -> Int -> Colour -> Canvas -> Canvas
  writePixel x y colour canvas =
    canvas // [(y, newRow)]
    where
      row = canvas ! y
      newRow = row // [(x, colour)]
      
  pixelAt :: Canvas -> Int -> Int -> Colour
  pixelAt canvas x y =
    row ! x
    where
      row = canvas ! y

  canvasToList :: Canvas -> [[Colour]]
  canvasToList canvas =
    elems (fmap elems canvas)
    
  cmap :: (Colour -> Colour) -> Canvas -> Canvas
  cmap f = fmap (fmap f)
  
  height :: Canvas -> Int
  height = length
  