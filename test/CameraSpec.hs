module CameraSpec where

  import Test.Hspec
  import Camera
  import TestUtil
  import Ray
  import Quad
  import Transforms
  import World
  import Canvas
  import Colour
  import Models

  spec :: Spec
  spec = do
    describe "Camera" $ do

      it "computes pixel size for a horizontal canvas" $ do
        let c = camera 200 125 (pi / 2)
        pixelSize c `shouldApproxBe` 0.01

      it "computes pixel size for a vertical canvas" $ do
        let c = camera 125 200 (pi / 2)
        pixelSize c `shouldApproxBe` 0.01

      it "constructs a ray through the center of the camera" $ do
        let c = camera 201 101 (pi/2)
        let r = rayForPixel 100 50 c
        origin r `shouldApproxBe` point 0 0 0
        direction r `shouldApproxBe` vector 0 0 (-1)

      it "constructs a ray through the corner of the camera" $ do
        let c = camera 201 101 (pi/2)
        let r = rayForPixel 0 0 c
        origin r `shouldApproxBe` point 0 0 0
        direction r `shouldApproxBe` vector 0.66519 0.33259 (-0.66851)

      it "constructs a ray when the camera is transformed" $ do
        let t = rotateY (pi / 4) `mul` translation 0 (-2) 5
        let c = setTransform (camera 201 101 (pi / 2)) t
        
        let r = rayForPixel 100 50 c
        origin r `shouldApproxBe` point 0 2 (-5)
        direction r `shouldApproxBe` vector (sqrt 2 / 2) 0 (-(sqrt 2 / 2))

      it "renders a world with a camera" $ do
        let w = defaultWorld
        let from = point 0 0 (-5)
        let to = point 0 0 0
        let up = vector 0 1 0
        
        let viewT = viewTransform from to up

        let c = setTransform (camera 11 11 (pi / 2)) viewT

        let image = render c w

        pixelAt image 5 5 `shouldApproxBe` Colour 0.38066 0.47583 0.2855
