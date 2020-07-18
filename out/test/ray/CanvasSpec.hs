module CanvasSpec
  where

  import Canvas
  import Test.Hspec
  import TestUtil

  spec :: Spec
  spec = do

    describe "Colour" $ do
      it "identifies the r component" $ do
        r (Colour (-0.5) 0.4 1.7) `shouldApproxBe` (-0.5)
      it "identifies the g component" $ do
        g (Colour (-0.5) 0.4 1.7) `shouldApproxBe` 0.4
      it "identifies the b component" $ do
        b (Colour (-0.5) 0.4 1.7) `shouldApproxBe` 1.7
      it "allows adding" $ do
        Colour 0.9 0.6 7.5 `add` Colour 0.7 0.1 0.25 `shouldApproxBe` Colour 1.6 0.7 7.75
      it "allows subtraction" $ do
        Colour 0.9 0.6 7.5 `minus` Colour 0.7 0.1 0.25 `shouldApproxBe` Colour 0.2 0.5 7.25
      it "allows scalar multiplication" $ do
        Colour 0.2 0.3 0.4 `mulScalar` 2 `shouldApproxBe` Colour 0.4 0.6 0.8
      it "allows colour multiplication" $ do
        Colour 1 0.2 0.4 `mulColour` Colour 0.9 1 0.1 `shouldApproxBe` Colour 0.9 0.2 0.04

    describe "Canvas" $ do
      it "is created with black pixles" $ do
        let canvas = buildCanvas 10 10
        let allPixels = [ pixelAt canvas x y | x <- [0 .. 9], y <- [0 .. 9]]
        allPixels `shouldApproxBe` replicate (10*10) black
      it "allows getting and setting of pixels" $ do
        let canvas = buildCanvas 6 4
        let newCanvas = writePixel 5 3 white canvas
        pixelAt newCanvas 5 3 `shouldApproxBe` white

    describe "PPM generation" $ do
      it "Writes a correct PPM header" $ do
        let canvas = buildCanvas 5 3
        let ppm = canvasToPPM canvas

        ppm `shouldStartWith` "P3\n5 3\n255"

      it "Writes in pixel data" $ do
        let blankCanvas = buildCanvas 5 3
        let c1 = Colour 1.5 0 0
        let c2 = Colour 0 0.5 0
        let c3 = Colour (-0.5) 0 1

        let pixelWrites = [ writePixel 0 0 c1, writePixel 2 1 c2, writePixel 4 2 c3 ]

        let canvas = foldl (\ c f -> f c) blankCanvas pixelWrites
        let ppm = canvasToPPM canvas

        ppm `shouldEndWith` unlines [
          "255 0 0 0 0 0 0 0 0 0 0 0 0 0 0",
          "0 0 0 0 0 0 0 128 0 0 0 0 0 0 0",
          "0 0 0 0 0 0 0 0 0 0 0 0 0 0 255"
          ]

      it "limits line lengths to 70 characters" $ do
        let c = Colour 1 0.8 0.6
        let blankCanvas = buildCanvas 10 2
        let canvas = cmap (const c) blankCanvas
        
        let ppm = canvasToPPM canvas
        
        ppm `shouldEndWith` unlines [
          "255 204 153 255 204 153 255 204 153 255 204 153 255 204 153 255 204",
          "153 255 204 153 255 204 153 255 204 153 255 204 153",
          "255 204 153 255 204 153 255 204 153 255 204 153 255 204 153 255 204",
          "153 255 204 153 255 204 153 255 204 153 255 204 153"
          ]