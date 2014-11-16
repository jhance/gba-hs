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

import           Test.HUnit hiding (assert)
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.HUnit (testCase)
import           Test.Tasty.QuickCheck (testProperty)
import           Test.QuickCheck
import           Test.QuickCheck.Monadic

--runTest :: forall a. (forall s. GBA s a) -> IO a
runTest :: GBA RealWorld a -> IO a
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

basic1 :: TestTree
basic1 = testProperty "write to on-board WRAM (8-bit)" . monadicIO $ do
    n <- pick arbitrary
    contents <- run . runTest $ do
        writeVirtual8 0x020000FF n
        readVirtual8 0x020000FF
    assert $ contents == n

basic2 :: TestTree
basic2 = testProperty "write to on-board WRAM (16-bit)" . monadicIO $ do
    n <- pick arbitrary
    contents <- run .runTest $ do
        writeVirtual16 0x02000100 n
        readVirtual16 0x02000100
    assert $ contents == n

basic3 :: TestTree
basic3 = testProperty "write to on-board WRAM (16-bit)" . monadicIO $ do
    n <- pick arbitrary
    contents <- run . runTest $ do
        writeVirtual32 0x02000100 n
        readVirtual32 0x02000100
    assert $ contents == n

basic4 :: TestTree
basic4 = testProperty "write to on-chip WRAM (8-bit)" . monadicIO $ do
    n <- pick arbitrary
    contents <- run . runTest $ do
        writeVirtual8 0x020000FF n
        readVirtual8 0x020000FF
    assert $ contents == n

bus1 :: TestTree
bus1 = testProperty "write with 8-bit, read with 16-bit" . monadicIO $ do
    m <- pick arbitrary
    l <- pick arbitrary
    contents <- run . runTest $ do
        writeVirtual8 0x02000000 l
        writeVirtual8 0x02000001 m
        readVirtual16 0x02000000
    assert $ contents == fromIntegral l `xor` shiftL (fromIntegral m) 8

bus2 :: TestTree
bus2 = testProperty "write with 16-bit, read with 8-bit" . monadicIO $ do
    n <- pick arbitrary
    (ls, ms) <- run . runTest $ do
        writeVirtual16 0x02000000 $ n
        (,) <$> readVirtual8 0x02000000 <*> readVirtual8 0x02000001
    assert $ ls == fromIntegral (shiftR (shiftL n 8) 8)
    assert $ ms == fromIntegral (shiftR n 8)

bus3 :: TestTree
bus3 = testProperty "write with 16-bit, read with 32-bit" . monadicIO $ do
    m <- pick arbitrary
    l <- pick arbitrary
    contents <- run . runTest $ do
        writeVirtual16 0x02000000 l
        writeVirtual16 0x02000002 m
        readVirtual32 0x02000000
    assert $ contents == fromIntegral l `xor` shiftL (fromIntegral m) 16