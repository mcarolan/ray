module TransformsSpec where

  import Test.Hspec
  import TestUtil
  import ApproxEqual
  import Quad
  import Transforms

  spec :: Spec
  spec = do
    describe "Transforms" $ do
      it "should be able to translate" $ do
        let transform = translation 5 (-3) 2
        let p = point (-3) 4 5

        transform `mul` p `shouldApproxBe` point 2 1 7
      it "should be able to translate by an inverse" $ do
        let transform = inverse (translation 5 (-3) 2)
        let p = point (-3) 4 5

        transform `mul` p `shouldApproxBe` point (-8) 7 3

      it "should be that translation does not affect vectors" $ do
        let transform = translation 5 (-3) 2
        let v = vector (-3) 4 5
        transform `mul` v `shouldApproxBe` v

      it "can scale a point" $ do
        let transform = scaling 2 3 4
        let p = point (-4) 6 8
        transform `mul` p `shouldApproxBe` point (-8) 18 32

      it "can scale a vector" $ do
        let transform = scaling 2 3 4
        let v = vector (-4) 6 8
        transform `mul` v `shouldApproxBe` vector (-8) 18 32

      it "can scale by an inverse" $ do
        let transform = inverse (scaling 2 3 4)
        let v = vector (-4) 6 8
        transform `mul` v `shouldApproxBe` vector (-2) 2 2


      it "allows scaling by a negative value (reflection)" $ do
        let transform = scaling (-1) 1 1
        let p = point 2 3 4

        transform `mul` p `shouldApproxBe` point (-2) 3 4

      it "allows rotation around the X axis" $ do
        let p = point 0 1 0
        let half_quarter = rotateX (pi / 4)
        let full_quarter = rotateX (pi / 2)

        half_quarter `mul` p `shouldApproxBe` point 0 (sqrt 2 / 2) (sqrt 2 / 2)
        full_quarter `mul` p `shouldApproxBe` point 0 0 1

      it "allows rotate around the X axis in the opposite direction" $ do
        let p = point 0 1 0
        let half_quarter = rotateX (pi / 4)

        inverse half_quarter `mul` p `shouldApproxBe` point 0 (sqrt 2 / 2) (-(sqrt 2 / 2))

      it "allows rotation around the Y axis" $ do
        let p = point 0 0 1
        let half_quarter = rotateY (pi / 4)
        let full_quarter = rotateY (pi / 2)

        half_quarter `mul` p `shouldApproxBe` point (sqrt 2 / 2) 0 (sqrt 2 / 2)
        full_quarter `mul` p `shouldApproxBe` point 1 0 0

      it "allows rotation around the Z axis" $ do
        let p = point 0 1 0
        let half_quarter = rotateZ (pi / 4)
        let full_quarter = rotateZ (pi / 2)

        half_quarter `mul` p `shouldApproxBe` point (-(sqrt 2) / 2) (sqrt 2 / 2) 0
        full_quarter `mul` p `shouldApproxBe` point (-1) 0 0

      it "allows shearing of x in proportion to y" $ do
        let p = point 2 3 4
        let transform = shearing 1 0 0 0 0 0
        transform `mul` p `shouldApproxBe` point 5 3 4

      it "allows shearing of x in proportion to z" $ do
        let p = point 2 3 4
        let transform = shearing 0 1 0 0 0 0
        transform `mul` p `shouldApproxBe` point 6 3 4

      it "allows shearing of y in proportion to x" $ do
        let p = point 2 3 4
        let transform = shearing 0 0 1 0 0 0
        transform `mul` p `shouldApproxBe` point 2 5 4

      it "allows shearing of y in proportion to z" $ do
        let p = point 2 3 4
        let transform = shearing 0 0 0 1 0 0
        transform `mul` p `shouldApproxBe` point 2 7 4

      it "allows shearing of z in proportion to x" $ do
        let p = point 2 3 4
        let transform = shearing 0 0 0 0 1 0
        transform `mul` p `shouldApproxBe` point 2 3 6

      it "allows shearing of z in proportion to y" $ do
        let p = point 2 3 4
        let transform = shearing 0 0 0 0 0 1
        transform `mul` p `shouldApproxBe` point 2 3 7

      it "Individual transformations are applied in sequence" $ do
        let p = point 1 0 1
        let a = rotateX (pi / 2)
        let b = scaling 5 5 5
        let c = translation 10 5 7

        let p1 = a `mul` p
        p1 `shouldApproxBe` point 1 (-1) 0

        let p2 = b `mul` p1
        p2 `shouldApproxBe` point 5 (-5) 0

        let p3 = c `mul` p2
        p3 `shouldApproxBe` point 15 0 7

      it "Chained transformations must be applied in reverse order" $ do
        let p = point 1 0 1
        let a = rotateX (pi / 2)
        let b = scaling 5 5 5
        let c = translation 10 5 7

        let t = c `mul` b `mul` a

        t `mul` p `shouldApproxBe` point 15 0 7