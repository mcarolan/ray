module TuplesSpec
where
  import Tuples
  import Test.Hspec
  import TestUtil

  spec :: Spec
  spec = do

    describe "Tuples" $ do
      it "identifies the x component" $ do
        tupleX (4.3, -4.2, 3.1, 1.0) `shouldApproxBe` 4.3

      it "identifies the y component" $ do
        tupleY (4.3, -4.2, 3.1, 1.0) `shouldApproxBe` (-4.2)

      it "identifies the z component" $ do
        tupleZ (4.3, -4.2, 3.1, 1.0) `shouldApproxBe` 3.1

      it "identifies the w component" $ do
        tupleW (4.3, -4.2, 3.1, 1.0) `shouldApproxBe` 1.0

      describe "when w component is 1.0" $ do
        it "identifies as a point" $ do
          tupleIsPoint (4.3, -4.2, 3.1, 1.0) `shouldBe` True

        it "does not identify as a vector" $ do
          tupleIsVector (4.3, -4.2, 3.1, 1.0) `shouldBe` False

      describe "when w component is 0.0" $ do
        it "does not identify as a point" $ do
          tupleIsPoint (4.3, -4.2, 3.1, 0.0) `shouldBe` False

        it "identifies as a vector" $ do
          tupleIsVector (4.3, -4.2, 3.1, 0.0) `shouldBe` True

      it "allows points to be constructed" $ do
        point 1.0 2.0 3.0 `shouldBe` (1.0, 2.0, 3.0, 1.0)

      it "allows vectors to be constructed" $ do
        vector 1.0 2.0 3.0 `shouldBe` (1.0, 2.0, 3.0, 0.0)