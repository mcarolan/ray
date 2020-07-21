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