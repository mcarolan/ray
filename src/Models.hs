module Models where
  import Data.Array hiding (elems)
  import Data.List (intercalate)

  data Material = Material {
  materialAmbient,
  materialDiffuse,
  materialSpecular,
  materialShininess,
  materialReflectivity,
  materialTransparency,
  materialRefractiveIndex :: Double,
  materialPattern :: Pattern } deriving (Show)

  defaultMaterial :: Material
  defaultMaterial = Material 0.1 0.9 0.9 200.0 0 0 1 (Constant white)

  data PointLight = PointLight { lightIntensity :: Colour, lightPosition :: Quad } deriving (Show)

  data Colour = Colour { r, g, b :: Double } deriving (Show)

  black = Colour 0 0 0
  white = Colour 1 1 1
  red = Colour 1 0 0
  blue = Colour 0 0 1
  green = Colour 0 1 0

  data Pattern = StripePattern { on, off :: Colour, patternTransform :: Matrix }
               | GradientPattern { from, to :: Colour, patternTransform :: Matrix }
               | RingPattern { on, off :: Colour, patternTransform :: Matrix }
               | CheckerPattern { on, off :: Colour, patternTransform :: Matrix }
               | Constant { c :: Colour } deriving (Show)

  data Quad = Quad { x, y, z, w :: Double } deriving (Show)

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

  newtype Matrix = Matrix { elems :: Array (Int, Int) Double}


  at :: Matrix -> (Int, Int) -> Double
  at m rc@(r, c)
    | r >= rows m || c >= columns m = error (show rc ++ " failed bounds check on " ++ show m)
    | otherwise = elems m ! rc

  rows :: Matrix -> Int
  rows m = fst (snd (bounds (elems m)))

  columns :: Matrix -> Int
  columns m = snd (snd (bounds (elems m)))

  instance Show Matrix where
    show m =
      show (rows m) ++ "x" ++ show (columns m) ++ " matrix:\n" ++
      numbers
      where
        rowNumbers r = intercalate "\t|\t" [ show (m `at` (r, c)) | c <- [0..columns m - 1]]
        allRows = [ rowNumbers r | r <- [0..rows m - 1]]
        numbers = unlines allRows
