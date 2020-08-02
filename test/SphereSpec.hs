module SphereSpec where

import Ray
import TestUtil
import Test.Hspec
import Quad
import Transforms

spec :: Spec
spec = do
  it "computes the normal on a sphere at a point on the X axis" $ do
    let shapeId = ShapeId 0
    let s = sphere shapeId
    normalAt s (point 1 0 0) `shouldApproxBe` vector 1 0 0
  it "computes the normal on a sphere at a point on the Y axis" $ do
    let shapeId = ShapeId 0
    let s = sphere shapeId
    normalAt s (point 0 1 0) `shouldApproxBe` vector 0 1 0
  it "computes the normal on a sphere at a point on the Z axis" $ do
    let shapeId = ShapeId 0
    let s = sphere shapeId
    normalAt s (point 0 0 1) `shouldApproxBe` vector 0 0 1
  it "computes the normal on a sphere at a point at a nonaxial point" $ do
    let shapeId = ShapeId 0
    let s = sphere shapeId
    let x = sqrt 3 / 3
    normalAt s (point x x x) `shouldApproxBe` vector x x x
  it "computes the normal on a sphere at a point at a nonaxial point" $ do
    let shapeId = ShapeId 0
    let s = sphere shapeId
    let x = sqrt 3 / 3
    let n = normalAt s (point x x x)
    n `shouldApproxBe` normalize n
  it "computes the normal on a translated sphere" $ do
    let shapeId = ShapeId 0
    let s = (sphere shapeId) { sphereTransform = translation 0 1 0 }
    let n = normalAt s (point 0 1.70711 (-0.70711))
    n `shouldApproxBe` vector 0 0.70711 (-0.70711)
  it "computes the normal on a transformed sphere" $ do
    let shapeId = ShapeId 0
    let m = scaling 1 0.5 1 `mul` rotateZ (pi / 5.0)
    let s = (sphere shapeId) { sphereTransform = m }
    let n = normalAt s (point 0 (sqrt 2 / 2) (-(sqrt 2 / 2)))
    n `shouldApproxBe` vector 0 0.97014 (-0.24254)