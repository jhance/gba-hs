module Main
where

import qualified Test.GBA.Thumb as T1
import qualified Test.GBA.Memory as T2
import           Test.Tasty
import           Test.Tasty.Ingredients.Rerun

main :: IO ()
main = defaultMainWithIngredients [rerunningTests defaultIngredients] tests

tests :: TestTree
tests = testGroup "testsuite" [T1.tests, T2.tests]