module Test.GBA.Thumb
    (tests)
where

import           Test.Tasty
import qualified Test.GBA.Thumb.T1 as T1
import qualified Test.GBA.Thumb.T2 as T2
import qualified Test.GBA.Thumb.T3 as T3
import qualified Test.GBA.Thumb.T4 as T4

tests :: TestTree
tests = testGroup "thumb"
            [ T1.tests
            , T2.tests
            , T3.tests
            , T4.tests
            ]