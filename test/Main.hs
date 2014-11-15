module Main
where

import qualified Test.GBA.Thumb.Instruction as T1
import           Test.Tasty

main = defaultMain (testGroup "" [T1.tests])