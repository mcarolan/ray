module CanvasPPMSpec where

  import Canvas
  import CanvasPPM
  import Test.Hspec
  import TestUtil
  import Colour
  import Models

  spec :: Spec
  spec = do
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
          
      it "terminates the PPM with a newline" $ do
        canvasToPPM (buildCanvas 5 3) `shouldEndWith` "\n"

      it "does not pack line when words less than limit" $ do
        let words = [ "foo", "bar" ]
        packLines words 7 `shouldBe` "foo bar\n"

      it "doe snot pack with single word less than limit" $ do
        let words = [ "foo" ]
        packLines words 7 `shouldBe` "foo\n"

      it "does pack line when words exceed the limit" $ do
        let words = [ "foo", "bar", "baz" ]
        packLines words 7 `shouldBe` "foo bar\nbaz\n"

      it "does pack line when words exceed the limit" $ do
        let words = [ "foo", "barz", "ba" ]
        packLines words 7 `shouldBe` "foo\nbarz ba\n"