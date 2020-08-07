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
