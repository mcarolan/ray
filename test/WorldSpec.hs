module WorldSpec
where
import Quad
import Test.Hspec
import TestUtil
import ApproxEqual
import World
import Lighting
import Quad
import Colour
import Ray
import Transforms
import Pattern

spec :: Spec
spec = do

  describe "World" $ do
    it "has a default" $ do
      let world = defaultWorld
      lights world `shouldApproxBe` [ PointLight white (point (-10) 10 (-10)) ]

      let material = defaultMaterial {
                  materialPattern = Constant (Colour 0.8 1.0 0.6),
                  materialDiffuse = 0.7,
                  materialSpecular = 0.2
                }

      let s1 = sphere {
        shapeMaterial = material
      }

      let s2 = sphere {
        shapeTransform = scaling 0.5 0.5 0.5
      }

      length (shapes world) `shouldBe` 2

      let f = head (shapes world)
      let s = head (tail (shapes world))

      fst f `shouldBe` ShapeId 0
      snd f `shouldApproxBe` s1

      fst s `shouldBe` ShapeId 1
      snd s `shouldApproxBe` s2

    it "intersects rays" $ do
      let world = defaultWorld
      let r = Ray (point 0 0 (-5)) (vector 0 0 1)
      let intersections = intersectWorld world r

      length intersections `shouldBe` 4

      map t intersections `shouldMatchList` [4, 4.5, 5.5, 6]

    it "shades an intersection" $ do
      let world = defaultWorld
      let r = Ray (point 0 0 (-5)) (vector 0 0 1)
      let shape = head (shapes world)
      let intersection = Intersection shape 4
      let comps = prepareComputations intersection r
      let c = shadeHit world comps

      c `shouldApproxBe` Colour 0.38066 0.47583 0.2855

    it "shades an intersection from the inside" $ do
      let world = setLight (PointLight white (point 0 0.25 0))  defaultWorld
      let r = Ray (point 0 0 0) (vector 0 0 1)
      let shape = head (tail (shapes world))
      let intersection = Intersection shape 0.5
      let comps = prepareComputations intersection r
      let c = shadeHit world comps

      c `shouldApproxBe` Colour 0.90498 0.90498 0.90498

  it "computes the colour when the ray misses" $ do
    let world = defaultWorld
    let r = Ray (point 0 0 (-5)) (vector 0 1 0)
    colourAt world r `shouldApproxBe` black

  it "computes the colour when a ray hits" $ do
    let world = defaultWorld
    let r = Ray (point 0 0 (-5)) (vector 0 0 1)
    colourAt world r `shouldApproxBe` Colour 0.38066 0.47583 0.2855

  it "computes the colour with an intersection behind the ray" $ do
    let world = defaultWorld
    let outerSphere = snd (head (shapes world))
    let outer = outerSphere {
      shapeMaterial = (shapeMaterial outerSphere) {
        materialAmbient = 1
      }
    }

    let innerSphere = snd (head (tail (shapes world)))
    let inner = innerSphere {
      shapeMaterial = (shapeMaterial innerSphere) {
        materialAmbient = 1
      }
    }

    let r = Ray (point 0 0 0.75) (vector 0 0 (-1))

    let newWorld = addShape inner (addShape outer (world { shapes = [] }))

    colourAt newWorld r `shouldApproxBe` white

  it "is not a shadow when nothing is collinear with the point or light" $ do
    let w = defaultWorld
    let p = point 0 10 0

    isShadowed w p `shouldBe` False

  it "is a shadow hwen there is an object between the point and the light" $ do
    let w = defaultWorld
    let p = point 10 (-10) 10
    isShadowed w p `shouldBe` True

  it "should not be in shadow when an object is behind the light" $ do
    let w = defaultWorld
    let p = point (-20) 20 (-20)
    isShadowed w p `shouldBe` False

  it "should not be in shadow when nothing is between the light and the point" $ do
    let w = defaultWorld
    let p = point (-2) (-2) (-2)
    isShadowed w p `shouldBe` False

  it "should shade correctly in a shadow" $ do
    let light = PointLight white (point 0 0 (-10))
    let s1 = sphere
    let s2 = sphere {
      shapeTransform = translation 0 0 10
    }

    let w = addShape s2 (addShape s1 (addLight light emptyWorld))
    let r = Ray (point 0 0 5) (vector 0 0 1)
    let i = Intersection (head (tail (shapes w))) 4
    let comps = prepareComputations i r

    let c = shadeHit w comps

    c `shouldApproxBe` Colour 0.1 0.1 0.1