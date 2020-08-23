module LightingSpec where

  import Test.Hspec
  import Lighting
  import TestUtil
  import Quad
  import Colour
  import Pattern
  import Models
  import ApproxEqual
  import Ray
 

  spec :: Spec
  spec = do
    it "lighting with eye between the light and the surface" $ do
      let m = defaultMaterial
      let position = point 0 0 0
      let eye = vector 0 0 (-1)
      let norm = vector 0 0 (-1)
      let light = PointLight white (point 0 0 (-10))
      let result = lighting m sphere light position eye norm False

      result `shouldApproxBe` Colour 1.9 1.9 1.9

    it "lighting with the surface in the shadow" $ do
          let m = defaultMaterial
          let position = point 0 0 0
          let eye = vector 0 0 (-1)
          let norm = vector 0 0 (-1)
          let light = PointLight white (point 0 0 (-10))
          let result = lighting m sphere light position eye norm True

          result `shouldApproxBe` Colour 0.1 0.1 0.1

    it "lighting with eye between the light and the surface, eye offset 45 degrees" $ do
      let m = defaultMaterial
      let position = point 0 0 0
      let eye = vector 0 (sqrt 2 / 2) (-(sqrt 2 / 2))
      let norm = vector 0 0 (-1)
      let light = PointLight white (point 0 0 (-10))
      let result = lighting m sphere light position eye norm False

      result `shouldApproxBe` Colour 1.0 1.0 1.0

    it "lighting with eye between the light and the surface, light offset 45 degrees" $ do
      let m = defaultMaterial
      let position = point 0 0 0
      let eye = vector 0 0 (-1)
      let norm = vector 0 0 (-1)
      let light = PointLight white (point 0 10 (-10))
      let result = lighting m sphere light position eye norm False

      result `shouldApproxBe` Colour 0.7364 0.7364 0.7364


    it "lighting with eye in path of the reflection vector" $ do
      let m = defaultMaterial
      let position = point 0 0 0
      let eye = vector 0 (-(sqrt 2 / 2)) (-(sqrt 2 / 2))
      let norm = vector 0 0 (-1)
      let light = PointLight white (point 0 10 (-10))
      let result = lighting m sphere light position eye norm False

      result `shouldApproxBe` Colour 1.6364 1.6364 1.6364


    it "lighting with the light behind the surfacer" $ do
      let m = defaultMaterial
      let position = point 0 0 0
      let eye = vector 0 0 (-1)
      let norm = vector 0 0 (-1)
      let light = PointLight white (point 0 0 10)
      let result = lighting m sphere light position eye norm False

      result `shouldApproxBe` Colour 0.1 0.1 0.1

    it "lighting with a pattern applied" $ do
      let m = defaultMaterial {
        materialPattern = stripePattern white black,
        materialAmbient = 1,
        materialSpecular = 0,
        materialDiffuse = 0
      }
      let eye = vector 0 0 (-1)
      let norm = vector 0 0 (-1)
      let light = PointLight white (point 0 0 (-10))
      let c1 = lighting m sphere light (point 0.9 0 0) eye norm False
      let c2 = lighting m sphere light (point 1.1 0 0) eye norm False

      c1 `shouldApproxBe` white
      c2 `shouldApproxBe` black