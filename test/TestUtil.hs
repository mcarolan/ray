module TestUtil
where

  import           Test.Hspec
  import           Control.Monad (unless)

  shouldApproxBe :: Double -> Double -> Expectation
  shouldApproxBe a b =
    unless ((abs (a - b)) < 0.00001) (expectationFailure (show a ++ " was not approx equal to " ++ show b))