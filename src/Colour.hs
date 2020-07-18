module Colour where
  data Colour = Colour { r, g, b :: Double } deriving (Show)

  addColour :: Colour -> Colour -> Colour
  addColour x y =
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
  red = Colour 1 0 0
