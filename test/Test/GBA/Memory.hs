{-# LANGUAGE RankNTypes #-}
module Test.GBA.Memory
    (tests)
where

import           Control.Applicative
import           Control.Lens
import           Control.Monad.ST
import           Data.Bits
import           Data.STRef
import           Data.Word
import           Numeric

import           Game.GBA.Boot
import           Game.GBA.CPUFlag
import           Game.GBA.Memory
import           Game.GBA.Memory.Real
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
        , bus4
        ]
    , testGroup "timer registers" $
        [ timer1
        , timer2
        , timer3
        , timer4
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

bus4 :: TestTree
bus4 = testProperty "write with 32-bit, read with 16-bit" . monadicIO $ do
    n <- pick arbitrary
    (ls, ms) <- run . runTest $ do
        writeVirtual32 0x02000000 $ n
        (,) <$> readVirtual16 0x02000000 <*> readVirtual16 0x02000002
    assert $ ls == fromIntegral (shiftR (shiftL n 16) 16)
    assert $ ms == fromIntegral (shiftR n 16)

timerTest :: Lens' (GBAContext RealWorld) (STRef RealWorld Word16)
          -> VirtualAddress -> TestTree
timerTest lns loc = testProperty ("write to timer 0x" ++ showHex loc "") . monadicIO $ do
    k <- pick arbitrary
    n <- pick arbitrary
    (mem, reg) <- run . runTest $ do
        writeReal16 (virtual loc) k
        writeVirtual16 loc n
        (,) <$> readVirtual16 loc <*> readLens lns
    assert $ mem == k
    assert $ reg == n

timer1 :: TestTree
timer1 = timerTest timerReload0 0x04000000

timer2 :: TestTree
timer2 = timerTest timerReload1 0x04000002

timer3 :: TestTree
timer3 = timerTest timerReload2 0x04000004

timer4 :: TestTree
timer4 = timerTest timerReload3 0x04000006
