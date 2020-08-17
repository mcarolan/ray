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

spec :: Spec
spec = do

  describe "World" $ do
    it "has a default" $ do
      let world = defaultWorld
      lights world `shouldApproxBe` [ PointLight white (point (-10) 10 (-10)) ]

      let material = defaultMaterial {
                  materialColour = Colour 0.8 1.0 0.6,
                  materialDiffuse = 0.7,
                  materialSpecular = 0.2
                }

      let s1 = sphere {
        sphereMaterial = material
      }

      let s2 = sphere {
        sphereTransform = scaling 0.5 0.5 0.5
      }

      length (spheres world) `shouldBe` 2

      let f = head (spheres world)
      let s = head (tail (spheres world))

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
      let shape = head (spheres world)
      let intersection = Intersection shape 4
      let comps = prepareComputations intersection r
      let c = shadeHit world comps

      c `shouldApproxBe` Colour 0.38066 0.47583 0.2855

    it "shades an intersection from the inside" $ do
      let world = setLight (PointLight white (point 0 0.25 0))  defaultWorld
      let r = Ray (point 0 0 0) (vector 0 0 1)
      let shape = head (tail (spheres world))
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
    let outerSphere = snd (head (spheres world))
    let outer = outerSphere {
      sphereMaterial = (sphereMaterial outerSphere) {
        materialAmbient = 1
      }
    }

    let innerSphere = snd (head (tail (spheres world)))
    let inner = innerSphere {
      sphereMaterial = (sphereMaterial innerSphere) {
        materialAmbient = 1
      }
    }

    let r = Ray (point 0 0 0.75) (vector 0 0 (-1))

    let newWorld = addSphere inner (addSphere outer (world { spheres = [] }))

    colourAt newWorld r `shouldApproxBe` materialColour (sphereMaterial innerSphere)