module RaySpec
where
import Quad
import Test.Hspec
import TestUtil
import ApproxEqual
import Ray
import Transforms

spec :: Spec
spec = do

  describe "Ray" $ do
    it "can compute the ray position over time" $ do
      let r = Ray (point 2 3 4) (vector 1 0 0)
      position r 0 `shouldApproxBe` point 2 3 4
      position r 1 `shouldApproxBe` point 3 3 4
      position r (-1) `shouldApproxBe` point 1 3 4
      position r 2.5 `shouldApproxBe` point 4.5 3 4
      
    it "can calculate intersection of a sphere at 2 points" $ do
      let r = Ray (point 0 0 (-5)) (vector 0 0 1)
      let shapeId = ShapeId 0
      let s = (shapeId, sphere)
      map t (r `intersect` s) `shouldApproxBe` [ 4.0, 6.0 ]
      map with (r `intersect` s) `shouldBe` [ shapeId, shapeId ]

    it "can calculate intersection of a sphere at a tangent" $ do
      let r = Ray (point 0 1 (-5)) (vector 0 0 1)
      let s = (ShapeId 0, sphere)
      map t (r `intersect` s) `shouldApproxBe` [ 5.0, 5.0 ]

    it "can show intersection when the ray misses the sphere" $ do
      let r = Ray (point 0 2 (-5)) (vector 0 0 1)
      let s = (ShapeId 0, sphere)
      map t (r `intersect` s) `shouldApproxBe` []

    it "can intersect when a ray starts inside a sphere" $ do
      let r = Ray (point 0 0 0) (vector 0 0 1)
      let s = (ShapeId 0, sphere)
      map t (r `intersect` s) `shouldApproxBe` [- 1, 1]

    it "can intersect when sphere is behind a ray" $ do
      let r = Ray (point 0 0 5) (vector 0 0 1)
      let s = (ShapeId 0, sphere)
      map t (r `intersect` s) `shouldApproxBe` [ -6, -4 ]

    it "calculates hit when all intersections have a positive t" $ do
      let shapeId = ShapeId 0
      let i1 = Intersection shapeId 1
      let i2 = Intersection shapeId 2
      hit [i2, i1] `shouldApproxBe` Just i1

    it "calculates hit when some intersections have a negative t" $ do
      let shapeId = ShapeId 0
      let i1 = Intersection shapeId (-1)
      let i2 = Intersection shapeId 1
      hit [i2, i1] `shouldApproxBe` Just i2

    it "calculates hit when all intersections have a negative t" $ do
      let shapeId = ShapeId 0
      let i1 = Intersection shapeId (-2)
      let i2 = Intersection shapeId (-1)
      hit [i2, i1] `shouldApproxBe` Nothing

    it "always takes the lowest nonnegative intersection" $ do
      let shapeId = ShapeId 0
      let i1 = Intersection shapeId 5
      let i2 = Intersection shapeId 7
      let i3 = Intersection shapeId (-3)
      let i4 = Intersection shapeId 2
      hit [i1, i2, i3, i4] `shouldApproxBe` Just i4

    it "allows translation of a ray" $ do
      let r = Ray (point 1 2 3) (vector 0 1 0)
      let m = translation 3 4 5
      let r2 = r `transform` m

      origin r2 `shouldApproxBe` point 4 6 8
      direction r2 `shouldApproxBe` vector 0 1 0

    it "allows scaling of a ray" $ do
      let r = Ray (point 1 2 3) (vector 0 1 0)
      let m = scaling 2 3 4
      let r2 = r `transform` m

      origin r2 `shouldApproxBe` point 2 6 12
      direction r2 `shouldApproxBe` vector 0 3 0

    it "intersects a scaled sphere with a ray" $ do
      let r = Ray (point 0 0 (-5)) (vector 0 0 1)
      let shapeId = ShapeId 0
      let s = sphere { sphereTransform = scaling 2 2 2 }
      map t (r `intersect` (shapeId, s)) `shouldApproxBe` [ 3, 7 ]

    it "intersects a translated sphere with a ray" $ do
      let r = Ray (point 0 0 (-5)) (vector 0 0 1)
      let shapeId = ShapeId 0
      let s = sphere { sphereTransform = translation 5 0 0 }
      map t (r `intersect` (shapeId, s)) `shouldApproxBe` []