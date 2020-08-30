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
import Models
import Test.Hspec.Tables


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
      let comps = prepareComputations intersection r [intersection]
      let c = shadeHit world comps 5

      c `shouldApproxBe` Colour 0.38066 0.47583 0.2855

    it "shades an intersection from the inside" $ do
      let world = setLight (PointLight white (point 0 0.25 0))  defaultWorld
      let r = Ray (point 0 0 0) (vector 0 0 1)
      let shape = head (tail (shapes world))
      let intersection = Intersection shape 0.5
      let comps = prepareComputations intersection r [intersection]
      let c = shadeHit world comps 5

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
    let comps = prepareComputations i r [i]

    let c = shadeHit w comps 5

    c `shouldApproxBe` Colour 0.1 0.1 0.1

  it "does not have a reflective colour for non reflective material" $ do
    let r = Ray (point 0 0 0) (vector 0 0 1)
    let s = sphere {
      shapeTransform = scaling 0.5 0.5 0.5,
      shapeMaterial = defaultMaterial {
        materialReflectivity = 0
      }
    }

    let w = addShape s (addLight defaultLight emptyWorld)
    let i = Intersection (ShapeId 1, s) 1
    let comps = prepareComputations i r [i]
    reflectedColour w comps 1 `shouldApproxBe` black

  it "calculates the reflectedColour for reflective material" $ do
    let s = plane {
      shapeMaterial = defaultMaterial {
        materialReflectivity = 0.5
      },
      shapeTransform = translation 0 (-1) 0
    }
    let w = addShape s defaultWorld
    let r = Ray (point 0 0 (-3)) (vector 0 (-(sqrt 2 / 2)) (sqrt 2 / 2))
    let i = Intersection (ShapeId 1, s) (sqrt 2)
    let comps = prepareComputations i r [i]
    reflectedColour w comps 1 `shouldApproxBe` Colour 0.19033 0.23791 0.14274

  it "calculates the reflectedColour for reflective material at max reflection depth" $ do
      let s = plane {
        shapeMaterial = defaultMaterial {
          materialReflectivity = 0.5
        },
        shapeTransform = translation 0 (-1) 0
      }
      let w = addShape s defaultWorld
      let r = Ray (point 0 0 (-3)) (vector 0 (-(sqrt 2 / 2)) (sqrt 2 / 2))
      let i = Intersection (ShapeId 1, s) (sqrt 2)
      let comps = prepareComputations i r [i]
      reflectedColour w comps 0 `shouldApproxBe` black

  it "calcaulates the shadeHit for reflective material" $ do
    let s = plane {
      shapeMaterial = defaultMaterial {
        materialReflectivity = 0.5
      },
      shapeTransform = translation 0 (-1) 0
    }
    let w = addShape s defaultWorld
    let r = Ray (point 0 0 (-3)) (vector 0 (-(sqrt 2 / 2)) (sqrt 2 / 2))
    let i = Intersection (ShapeId 1, s) (sqrt 2)
    let comps = prepareComputations i r [i]
    shadeHit w comps 1 `shouldApproxBe` Colour 0.87675 0.92434 0.82917

  it "deals with mutually reflective surfaces" $ do
    let l = PointLight white (point 0 0 0)
    let w = addLight l emptyWorld
    let lower = plane {
      shapeMaterial = defaultMaterial {
        materialReflectivity = 1
      },
      shapeTransform = translation 0 (-1) 0
    }
    let upper = plane {
      shapeMaterial = defaultMaterial {
        materialReflectivity = 1
      },
      shapeTransform = translation 0 1 0
    }

    let fullWorld = addShape lower (addShape upper (addLight l emptyWorld))
    let r = Ray (point 0 0 0) (vector 0 1 0)

    colourAt fullWorld r `shouldApproxBe` Colour 11.4 11.4 11.4

  describe "finds n1 and n2 at various intersections" $
    byExample
      ("index", "expected n1", "expected n2")
      [
        (0, 1.0, 1.5),
        (1, 1.5, 2.0),
        (2, 2.0, 2.5),
        (3, 2.5, 2.5),
        (4, 2.5, 1.5),
        (5, 1.5, 1.0)
      ]
      (\index expectedN1 expectedN2 ->
        let a = (ShapeId 0, glassSphere {
                  shapeTransform = scaling 2 2 2,
                  shapeMaterial = defaultMaterial {
                    materialRefractiveIndex = 1.5
                  }
                })
            b = (ShapeId 1, glassSphere {
                    shapeTransform = translation 0 0 (-0.25),
                    shapeMaterial = defaultMaterial {
                      materialRefractiveIndex = 2
                    }
                  }
                )
            c = (ShapeId 2, glassSphere {
              shapeTransform = translation 0 0 0.25,
              shapeMaterial = defaultMaterial {
                materialRefractiveIndex = 2.5
              }
            })
            r = Ray (point 0 0 (-4)) (vector 0 0 1)
            xs = [ Intersection a 2, Intersection b 2.75, Intersection c 3.25, Intersection b 4.75, Intersection c 5.25, Intersection a 6 ]
            h = xs !! index
            comps = prepareComputations h r xs
            n1ApproxCorrect = n1 comps `approxEqual` expectedN1
            n2ApproxCorrect = n2 comps `approxEqual` expectedN2
      in
        n1ApproxCorrect && n2ApproxCorrect `shouldBe` True
      )

  it "finds the refracted colour of an opaque surface" $ do
    let s = head (shapes defaultWorld)
    let r = Ray (point 0 0 (-5)) (vector 0 0 1)
    let xs = [Intersection s 4, Intersection s 6]
    let comps = prepareComputations (Intersection s 4) r xs

    refractedColour defaultWorld comps 5 `shouldApproxBe` black
    
  it "finds the refracted colour of at max recursion depth" $ do
    let l = PointLight white (point 0 0 0)
    let w = addShape glassSphere (addLight l emptyWorld)
    let s = head (shapes w)

    let r = Ray (point 0 0 (-5)) (vector 0 0 1)
    let xs = [Intersection s 4, Intersection s 6]
    let comps = prepareComputations (Intersection s 4) r xs

    refractedColour w comps 0 `shouldApproxBe` black

  it "finds the refracted colour with total internal reflection" $ do
    let l = PointLight white (point 0 0 0)
    let s = sphere {
      shapeMaterial = defaultMaterial {
        materialTransparency = 1,
        materialRefractiveIndex = 1.5
      }
    }

    let w = addShape s (addLight l emptyWorld)
    let r = Ray (point 0 0 (sqrt 2 / 2)) (vector 0 1 0)
    let xs = [Intersection (head (shapes w)) (-(sqrt 2) / 2), Intersection (head (shapes w)) (sqrt 2 / 2)]
    let comps = prepareComputations (xs !! 1) r xs
    refractedColour w comps 5 `shouldApproxBe` black

