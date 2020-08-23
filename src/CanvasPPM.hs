module CanvasPPM where

  import Canvas
  import Colour
  import Data.Array
  import Models

  canvasPPMHeader :: Canvas -> String
  canvasPPMHeader canvas =
    "P3\n" ++ show width ++ " " ++ show height ++ "\n255"
        where
          height = length canvas
          width = length (canvas ! 0)

  canvasPPMRow :: [Colour] -> String
  canvasPPMRow row =
    packLines (concatMap pixel row) 70
    where
      clamp x = min (max 0 x) 255
      component x = show (clamp (round (x * 255)))
      pixel c = [ component (r c), component (g c), component (b c) ]

  canvasToPPM :: Canvas -> String
  canvasToPPM canvas =
    canvasPPMHeader canvas ++
    "\n" ++
    concatMap canvasPPMRow canvasList
    where canvasList = canvasToList canvas

  packLines :: [String] -> Int -> String
  packLines parts maxLength =
    inner [] "" parts
    where
      inner acc "" (w:ws) =
        inner acc w ws
      inner acc curr (w:ws) | (length curr + length w + 1) <= maxLength =
        inner acc (curr ++ " " ++ w) ws
      inner acc curr (w:ws) =
        inner (acc ++ [curr]) w ws
      inner acc "" _ =
        unlines acc
      inner acc curr _ =
        unlines (acc ++ [curr])