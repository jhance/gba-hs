{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
-- | This module contains essentially the
-- most primitive items which rely on the
-- ghc monad.
module Game.GBA.Monad
    ( GBA
    , GBAContext
    , makeGBAContext
    , runGBA
    -- * General context lens operations
    , readLens
    , writeLens
    -- * Lenses
    , gbaRegisters
    , gbaMemoryMap
    -- ** Timer lenses
    , timerReload0
    , timerReload1
    , timerReload2
    , timerReload3
    )
where

import           Control.Applicative
import           Control.Lens
import           Control.Monad.Base
import           Control.Monad.Reader
import           Control.Monad.ST
import           Data.Bits
import           Data.Word
import           Data.STRef

import           Game.GBA.RegisterSet
import           Game.GBA.MemoryMap

data GBAContext s = GBAContext {
        _gbaRegisters :: RegisterSet s
      , _gbaMemoryMap :: MemoryMap s
      , _gbaTimerReloads :: TimerReloadSet s
    }

-- | When you write to a timer virtual memory location,
-- the timer starts counting from that. But that item
-- also gets duplicated - so we need to store it somewhere
-- else.
--
-- Essentially, these are written to whenever the addresses
-- 0x0400 0100, 0x0400 0104, 0x0040 0108, 0x004010C
-- are written to. (Handled by @writeVirtual@)
data TimerReloadSet s = TimerReloadSet {
        _timerReload0' :: STRef s Word16
      , _timerReload1' :: STRef s Word16
      , _timerReload2' :: STRef s Word16
      , _timerReload3' :: STRef s Word16
    }

makeLenses ''GBAContext
makeLenses ''TimerReloadSet

timerReload0 :: Lens' (GBAContext s) (STRef s Word16)
timerReload0 = gbaTimerReloads . timerReload0'

timerReload1 :: Lens' (GBAContext s) (STRef s Word16)
timerReload1 = gbaTimerReloads . timerReload1'

timerReload2 :: Lens' (GBAContext s) (STRef s Word16)
timerReload2 = gbaTimerReloads . timerReload2'

timerReload3 :: Lens' (GBAContext s) (STRef s Word16)
timerReload3 = gbaTimerReloads . timerReload3'

makeTimerReloadSet :: ST s (TimerReloadSet s)
makeTimerReloadSet = TimerReloadSet
    <$> newSTRef 0 <*> newSTRef 0 <*> newSTRef 0 <*> newSTRef 0

-- | Creates an empty context.
makeGBAContext :: ST s (GBAContext s)
makeGBAContext = GBAContext
    <$> makeRegisterSet
    <*> makeMemoryMap
    <*> makeTimerReloadSet

readLens :: Lens' (GBAContext s) (STRef s a) -> GBA s a
readLens lens = view lens >>= liftBase . readSTRef

writeLens :: Lens' (GBAContext s) (STRef s a) -> a -> GBA s ()
writeLens lens val = view lens >>= liftBase . flip writeSTRef val

newtype GBA s a = GBA (ReaderT (GBAContext s) (ST s) a)
    deriving (Functor, Applicative, Monad, MonadReader (GBAContext s),
              MonadBase (ST s))

runGBA :: GBAContext s -> GBA s a -> ST s a
runGBA context (GBA gba) = runReaderT gba context

--type GBA s a = ReaderT (GBAContext s) (ST s) a
