module TestUtil
where

  import           Test.Hspec
  import           Control.Monad (unless)
  import           Quad

  shouldApproxBe :: (Show a, ApproxEqual a) => a -> a -> Expectation
  shouldApproxBe a b =
    unless (approxEqual a b) (expectationFailure (show a ++ " was not approx equal to " ++ show b))