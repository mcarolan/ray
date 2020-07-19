module MatrixSpec where

  import Test.Hspec
  import TestUtil
  import ApproxEqual
  import Quad

  spec :: Spec
  spec = do
    describe "Matrices" $ do
      it "should be able to construct a 4x4 matrix" $ do
        let m = matrix4 1     2     3     4
                        5.5   6.5   7.5   8.5
                        9     10    11    12
                        13.5  14.5  15.5  16.5
                        
        rows m `shouldBe` 4
        columns m `shouldBe` 4

        (m `at` (0, 0)) `shouldApproxBe` 1
        (m `at` (0, 3)) `shouldApproxBe` 4
        (m `at` (1, 0)) `shouldApproxBe` 5.5
        (m `at` (1, 2)) `shouldApproxBe` 7.5
        (m `at` (2, 2)) `shouldApproxBe` 11
        (m `at` (3, 0)) `shouldApproxBe` 13.5
        (m `at` (3, 2)) `shouldApproxBe` 15.5

      it "should be able to construct a 2x2 matrix" $ do
        let m = matrix2 (-3) 5
                        1    (-2)
                        
        rows m `shouldBe` 2
        columns m `shouldBe` 2

        (m `at` (0, 0)) `shouldApproxBe` (-3)
        (m `at` (0, 1)) `shouldApproxBe` 5
        (m `at` (1, 0)) `shouldApproxBe` 1
        (m `at` (1, 1)) `shouldApproxBe` (-2)

      it "should be able to construct a 3x3 matrix" $ do
        let m = matrix3 (-3)    5     0
                        1       (-2)  (-7)
                        0       1     1

        rows m `shouldBe` 3
        columns m `shouldBe` 3

        (m `at` (0, 0)) `shouldApproxBe` (-3)
        (m `at` (1, 1)) `shouldApproxBe` (-2)
        (m `at` (2, 2)) `shouldApproxBe` 1

      it "should have approximate equality" $ do
        let a = matrix4 1   2   3   4
                        5   6   7   8
                        9   10  11  12
                        13  14  15  16

        (a `approxEqual` a) `shouldBe` True

      it "should have approximate inequality" $ do
        let a = matrix4 1   2   3   4
                        5   6   7   8
                        9   10  11  12
                        13  14  15  16

        let b = matrix4 9   2   3   4
                        5   6   7   8
                        9   10  11  12
                        13  14  15  16

        (a `approxEqual` b) `shouldBe` False

        (a `approxEqual` a) `shouldBe` True

      it "should allow matrix multiplication" $ do
        let a = matrix4 1   2   3   4
                        5   6   7   8
                        9   8   7   6
                        5   4   3   2

        let b = matrix4 (-2)    1   2   3
                        3       2   1   (-1)
                        4       3   6   5
                        1       2   7   8

        a `matmul` b `shouldApproxBe`
          matrix4 20    22    50    48
                  44    54    114   108
                  40    58    110   102
                  16    26    46    42

      it "should allow matrix tuple multiplication" $ do
        let a = matrix4 1   2   3   4
                        2   4   4   2
                        8   6   4   1
                        0   0   0   1

        let b = Quad 1 2 3 1

        (a `mattupmul` b) `shouldApproxBe` Quad 18 24 33 1

      it "should allow conversion between quad and matrix" $ do
        let q = Quad 1 2 3 4
        let m = matrixFromQuad q

        rows m `shouldBe` 4
        columns m `shouldBe` 1
        quadFromMatrix m `shouldApproxBe` q