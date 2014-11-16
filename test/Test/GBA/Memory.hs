{-# LANGUAGE RankNTypes #-}
module Test.GBA.Memory
    (tests)
where

import           Control.Monad.ST

import           Game.GBA.Boot
import           Game.GBA.CPUFlag
import           Game.GBA.Memory
import           Game.GBA.Monad

import           Test.HUnit
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.HUnit (testCase)

runTest :: (forall s. GBA s a) -> IO a
runTest gba = stToIO $ do
    context <- makeGBAContext
    runGBA context (bootForTest Thumb >> gba)

tests = testGroup "memory" $
    [ testGroup "basic" $
        [ basic1
        , basic2
        ]
    , testGroup "timer registers" $
        [
        ]
    ]

basic1 :: TestTree
basic1 = testCase "write to on-board WRAM (8-bit)" $ do
    contents <- runTest $ do
        writeVirtual8 0x020000FF 5
        readVirtual8 0x020000FF
    contents @?= 5

basic2 :: TestTree
basic2 = testCase "write to on-chip WRAM (8-bit)" $ do
    contents <- runTest $ do
        writeVirtual8 0x020000FF 5
        readVirtual8 0x020000FF
    contents @?= 5
