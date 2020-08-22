module PatternSpec where
import Test.Hspec
import TestUtil
import ApproxEqual
import Pattern
import Quad
import Colour
import Transforms
import Ray

spec :: Spec
spec = do

  describe "Pattern" $ do
    it "is constant in y" $ do
      patternColourAt (stripePattern white black) (point 0 0 0) `shouldApproxBe` white
      patternColourAt (stripePattern white black) (point 0 1 0) `shouldApproxBe` white
      patternColourAt (stripePattern white black) (point 0 2 0) `shouldApproxBe` white

    it "is constant in z" $ do
      patternColourAt (stripePattern white black) (point 0 0 0) `shouldApproxBe` white
      patternColourAt (stripePattern white black) (point 0 0 1) `shouldApproxBe` white
      patternColourAt (stripePattern white black) (point 0 0 2) `shouldApproxBe` white

    it "alternates in x" $ do
      patternColourAt (stripePattern white black) (point 0 0 0) `shouldApproxBe` white
      patternColourAt (stripePattern white black) (point 0.9 0 0) `shouldApproxBe` white
      patternColourAt (stripePattern white black) (point 1 0 0) `shouldApproxBe` black
      patternColourAt (stripePattern white black) (point (-0.1) 0 0) `shouldApproxBe` black
      patternColourAt (stripePattern white black) (point (-1) 0 0) `shouldApproxBe` black
      patternColourAt (stripePattern white black) (point (-1.1) 0 0) `shouldApproxBe` white

    it "stripes with an object transformation" $ do
      let s = sphere {
        shapeTransform = scaling 2 2 2
      }
      let p = stripePattern white black
      patternColourForObject p s (point 1.5 0 0) `shouldApproxBe` white


    it "stripes with a pattern transformation" $ do
      let s = sphere
      let p = (stripePattern white black) {
        patternTransform = scaling 2 2 2
      }
      patternColourForObject p s (point 1.5 0 0) `shouldApproxBe` white

    it "stripes with an object and pattern transformation" $ do
      let s = sphere {
        shapeTransform = scaling 2 2 2
      }
      let p = (stripePattern white black) {
        patternTransform = translation 0.5 0 0
      }
      patternColourForObject p s (point 2.5 0 0) `shouldApproxBe` white
