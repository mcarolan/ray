module Canvas
  where

  import Data.Array
  import Data.List
  
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

  canvasPPMHeader :: Canvas -> String
  canvasPPMHeader canvas =
    "P3\n" ++ show width ++ " " ++ show height ++ "\n255"
        where
          height = length canvas
          width = length (canvas ! 0)

  canvasPPMRow :: [Colour] -> String
  canvasPPMRow row =
    unwords (map pixel row)
    where
      clamp x = min (max 0 x) 255
      component x = show (clamp (round (x * 255)))
      pixel c = component (r c) ++ " " ++ component (g c) ++ " " ++ component (b c)

  canvasToPPM :: Canvas -> String
  canvasToPPM canvas =
    canvasPPMHeader canvas ++
    "\n" ++
    unlines (map canvasPPMRow canvasList)
    where canvasList = canvasToList canvas


  packLinesInner :: String -> String -> [String] -> Int -> String
  packLinesInner acc curr w:ws maxLength
    | (length curr + length w) < maxLength = packLinesInner acc  
    
  packLines :: [String] -> Int -> String
  packLines parts maxLength =
    inner "" "" parts
    where 
      inner acc line x:xs | (length line + length x) < maxLength = inner acc (line ++ x) xs
    
  cmap :: (Colour -> Colour) -> Canvas -> Canvas
  cmap f = fmap (fmap f)