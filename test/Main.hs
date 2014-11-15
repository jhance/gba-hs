module Main
where

import qualified Test.GBA.Thumb as T1
import           Test.Tasty

main = defaultMain (testGroup "testsuite" [T1.tests])