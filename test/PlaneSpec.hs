module PlaneSpec where
import Test.Hspec
import TestUtil
import ApproxEqual
import Ray
import Quad

spec :: Spec
spec = do

  describe "Plane" $ do
    it "has a constant normal everywhere" $ do
      localNormalAt plane (point 0 0 0) `shouldApproxBe` vector 0 1 0
      localNormalAt plane (point 10 0 (-100)) `shouldApproxBe` vector 0 1 0
      localNormalAt plane (point (-5) 0 150) `shouldApproxBe` vector 0 1 0

    it "has no intersections with the ray parallel" $ do
      let r = Ray (point 0 10 0) (vector 0 0 1)
      localIntersectAt (ShapeId 0, plane) r `shouldBe` []

    it "has no intersections with a coplanar ray" $ do
      let r = Ray (point 0 0 0) (vector 0 0 1)
      localIntersectAt (ShapeId 0, plane) r `shouldBe` []

    it "intersects a plane from above" $ do
      let r = Ray (point 0 1 0) (vector 0 (-1) 0)
      let shape = (ShapeId 0, plane)
      localIntersectAt shape r `shouldBe` [ Intersection shape 1 ]

    it "intersects a plane from below" $ do
      let r = Ray (point 0 (-1) 0) (vector 0 1 0)
      let shape = (ShapeId 0, plane)
      localIntersectAt shape r `shouldBe` [ Intersection shape 1 ]