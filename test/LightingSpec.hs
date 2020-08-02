module LightingSpec where

  import Test.Hspec
  import Lighting
  import TestUtil
  import Quad
  import Colour

  spec :: Spec
  spec = do
    it "lighting with eye between the light and the surface" $ do
      let m = defaultMaterial
      let position = point 0 0 0
      let eye = vector 0 0 (-1)
      let norm = vector 0 0 (-1)
      let light = PointLight white (point 0 0 (-10))
      let result = lighting m light position eye norm

      result `shouldApproxBe` Colour 1.9 1.9 1.9


    it "lighting with eye between the light and the surface, eye offset 45 degrees" $ do
      let m = defaultMaterial
      let position = point 0 0 0
      let eye = vector 0 (sqrt 2 / 2) (-(sqrt 2 / 2))
      let norm = vector 0 0 (-1)
      let light = PointLight white (point 0 0 (-10))
      let result = lighting m light position eye norm

      result `shouldApproxBe` Colour 1.0 1.0 1.0

    it "lighting with eye between the light and the surface, light offset 45 degrees" $ do
      let m = defaultMaterial
      let position = point 0 0 0
      let eye = vector 0 0 (-1)
      let norm = vector 0 0 (-1)
      let light = PointLight white (point 0 10 (-10))
      let result = lighting m light position eye norm

      result `shouldApproxBe` Colour 0.7364 0.7364 0.7364


    it "lighting with eye in path of the reflection vector" $ do
      let m = defaultMaterial
      let position = point 0 0 0
      let eye = vector 0 (-(sqrt 2 / 2)) (-(sqrt 2 / 2))
      let norm = vector 0 0 (-1)
      let light = PointLight white (point 0 10 (-10))
      let result = lighting m light position eye norm

      result `shouldApproxBe` Colour 1.6364 1.6364 1.6364


    it "lighting with the light behind the surfacer" $ do
      let m = defaultMaterial
      let position = point 0 0 0
      let eye = vector 0 0 (-1)
      let norm = vector 0 0 (-1)
      let light = PointLight white (point 0 0 10)
      let result = lighting m light position eye norm

      result `shouldApproxBe` Colour 0.1 0.1 0.1