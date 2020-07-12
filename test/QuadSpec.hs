module QuadSpec
where
  import Quad
  import Test.Hspec
  import TestUtil

  spec :: Spec
  spec = do

    describe "Quad" $ do
      it "identifies the x component" $ do
        x (Quad (4.3 :: Double) (-4.2) 3.1 1.0) `shouldApproxBe` 4.3

      it "identifies the y component" $ do
        y (Quad (4.3 :: Double) (-4.2) 3.1 1.0) `shouldApproxBe` (-4.2)

      it "identifies the z component" $ do
        z (Quad (4.3 :: Double) (-4.2) 3.1 1.0) `shouldApproxBe` 3.1

      it "identifies the w component" $ do
        w (Quad (4.3 :: Double) (-4.2) 3.1 1.0) `shouldApproxBe` 1.0

      describe "when w component is 1.0" $ do
        it "identifies as a point" $ do
          isPoint (Quad 4.3 (-4.2) 3.1 1.0) `shouldBe` True

        it "does not identify as a vector" $ do
          isVector (Quad 4.3 (-4.2) 3.1 1.0) `shouldBe` False

      describe "when w component is 0.0" $ do
        it "does not identify as a point" $ do
          isPoint (Quad 4.3 (-4.2) 3.1 0.0) `shouldBe` False

        it "identifies as a vector" $ do
          isVector (Quad 4.3 (-4.2) 3.1 0.0) `shouldBe` True

      it "allows points to be constructed" $ do
        (point (1.0 :: Double) 2.0 3.0) `shouldApproxBe` (Quad 1.0 2.0 3.0 1.0)

      it "allows vectors to be constructed" $ do
        (vector (1.0 :: Double) 2.0 3.0) `shouldApproxBe` (Quad 1.0 2.0 3.0 0.0)

      describe "approximate equality" $ do
        it "considers 1.1 and 1.09999999 equal" $ do
          approxEqual (1.1 :: Double) 1.09999999 `shouldBe` True
        it "does not consider 1.1 and 1.09 equal" $ do
          approxEqual (1.1 :: Double) 1.09 `shouldBe` False

      describe "operations" $ do
        it "can be added" $ do
          let a = Quad (3 :: Double) (-2) 5 1
          let b = Quad (-2) 3 1 0
          let expected = Quad 1 1 6 1
          a `add` b `shouldApproxBe` expected

        it "can be subtracted" $ do
          let a = point (3 :: Double) 2 1
          let b = point 5 6 7
          let expected = vector (-2) (-4) (-6)
          a `minus` b `shouldApproxBe` expected

        it "can subtract a vector from a point" $ do
          let a = point (3 :: Double) 2 1
          let b = vector 5 6 7
          let expected = point (-2) (-4) (-6)
          a `minus` b `shouldApproxBe` expected

        it "can subtract a vector from a vector" $ do
          let a = vector (3 :: Double) 2 1
          let b = vector 5 6 7
          let expected = vector (-2) (-4) (-6)
          a `minus` b `shouldApproxBe` expected

        it "can negate a quad" $ do
          let a = Quad (-1 :: Double) (-2) (-3) (-4)
          let expected = Quad 1 2 3 4
          neg a `shouldApproxBe` expected

        it "can multiply a quad by a scalar" $ do
          let a = Quad (1 :: Double) (-2) 3 (-4)
          let expected = Quad 3.5 (-7) 10.5 (-14)
          a `mul` 3.5 `shouldApproxBe` expected

        it "can divde a quad by a scalar" $ do
          let a = Quad (1 :: Double) (-2) 3 (-4)
          let expected = Quad 0.5 (-1) 1.5 (-2)
          a `divide` 2 `shouldApproxBe` expected

      describe "magnitude" $ do
        it "computes the magnitude of vector(1, 0, 0)" $ do
          magnitude (vector (1 :: Double) 0 0) `shouldApproxBe` 1
        it "computes the magnitude of vector(0, 1, 0)" $ do
          magnitude (vector (0 :: Double) 1 0) `shouldApproxBe` 1
        it "computes the magnitude of vector(0, 0, 1)" $ do
          magnitude (vector (0 :: Double) 0 1) `shouldApproxBe` 1
        it "computes the magnitude of vector(1, 2, 3)" $ do
          magnitude (vector (1 :: Double) 2 3) `shouldApproxBe` (sqrt 14)
        it "computes the magnitude of vector(-1, -2, -3)" $ do
          magnitude (vector (-1 :: Double) (-2) (-3)) `shouldApproxBe` (sqrt 14)

      describe "normalize" $ do
        it "normalizes vector(4, 0, 0) to vector(1, 0, 0)" $ do
          normalize (vector (4 :: Double) 0 0) `shouldApproxBe` vector 1 0 0
        it "normalizes vector(1, 2, 3)" $ do
          normalize (vector (1 :: Double) 2 3) `shouldApproxBe` vector 0.26726 0.53452 0.80178
        it "magnitude of a normalized vector is 1" $ do
          magnitude (normalize (vector (1 :: Double) 2 3)) `shouldApproxBe` 1

      describe "dot" $ do
        it "gives the dot product of 2 vectors" $ do
          (vector (1 :: Double) 2 3) `dot` (vector 2 3 4) `shouldApproxBe` 20

      describe "cross" $ do
        it "is (-1, 2, -1) for (1, 2, 3) cross (2, 3, 4)" $ do
          (vector (1 :: Double) 2 3) `cross` (vector 2 3 4) `shouldApproxBe` (vector (-1) 2 (-1))
        it "is (1, -2, 1) for (2, 3, 4) cross (1, 2, 3)" $ do
           (vector (2 :: Double) 3 4) `cross` (vector 1 2 3) `shouldApproxBe` (vector 1 (-2) 1)
