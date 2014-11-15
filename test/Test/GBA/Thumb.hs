module Test.GBA.Thumb
    (tests)
where

import           Test.Tasty
import qualified Test.GBA.Thumb.Instruction as T1
import qualified Test.GBA.Thumb.Execution as T2

tests :: TestTree
tests = testGroup "thumb instructions" $
            [ T1.tests
            , T2.tests
            ]