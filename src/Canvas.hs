module Canvas
  where

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