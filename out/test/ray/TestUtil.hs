module TestUtil
where

  import           Test.Hspec
  import           Control.Monad (unless)
  import           ApproxEqual
  import           Data.List
  
  shouldApproxBe :: (Show a, ApproxEqual a) => a -> a -> Expectation
  shouldApproxBe a b =
    unless (approxEqual a b) (expectationFailure (show a ++ " was not approx equal to " ++ show b))
    
--  shouldStartWith :: String -> String -> Expectation
--  shouldStartWith str prefix =
--    unless (isPrefixOf prefix str) (expectationFailure ("[" ++ prefix ++ "] was not a prefix of [" ++ str ++ "]"))