module Main
where

import qualified Test.GBA.Thumb as T1
import qualified Test.GBA.Memory as T2
import           Test.Tasty

main = defaultMain (testGroup "testsuite" [T1.tests, T2.tests])