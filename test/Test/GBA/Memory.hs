{-# LANGUAGE RankNTypes #-}
module Test.GBA.Memory
    (tests)
where

import           Control.Applicative
import           Control.Monad.ST
import           Data.Bits
import           Data.Word

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
        , basic3
        ]
    , testGroup "bus variance" $
        [ bus1
        , bus2
        , bus3
        ]
    , testGroup "timer registers" $
        [
        ]
    ]

-- most of these basic ones can probably be quickcheckified
basic1 :: TestTree
basic1 = testCase "write to on-board WRAM (8-bit)" $ do
    contents <- runTest $ do
        writeVirtual8 0x020000FF 5
        readVirtual8 0x020000FF
    contents @?= 5

basic2 :: TestTree
basic2 = testCase "write to on-board WRAM (16-bit/small)" $ do
    contents <- runTest $ do
        writeVirtual16 0x02000100 5
        readVirtual16 0x02000100
    contents @?= 5

basic3 :: TestTree
basic3 = testCase "write to on-board WRAM (16-bit/large)" $ do
    contents <- runTest $ do
        writeVirtual16 0x02000100 $ 5 `xor` 2^9
        readVirtual16 0x02000100
    contents @?= 5 `xor` 2^9

basic4 :: TestTree
basic4 = testCase "write to on-chip WRAM (8-bit)" $ do
    contents <- runTest $ do
        writeVirtual8 0x020000FF 5
        readVirtual8 0x020000FF
    contents @?= 5

bus1 :: TestTree
bus1 = testCase "write with 8-bit, read with 16-bit" $ do
    contents <- runTest $ do
        writeVirtual8 0x02000000 5
        writeVirtual8 0x02000001 18
        readVirtual16 0x02000000
    contents @?= 5 `xor` shiftL 18 8

bus2 :: TestTree
bus2 = testCase "write with 16-bit, read with 8-bit" $ do
    (ls, ms) <- runTest $ do
        writeVirtual16 0x02000000 $ 5 `xor` shiftL 18 8
        (,) <$> readVirtual8 0x02000000 <*> readVirtual8 0x02000001
    ls @?= 5
    ms @?= 18

bus3 :: TestTree
bus3 = testCase "write with 16-bit, read with 32-bit" $ do
    contents <- runTest $ do
        writeVirtual16 0x02000000 5
        writeVirtual16 0x02000002 18
        readVirtual32 0x02000000
    contents @?= 5 `xor` shiftL 18 16