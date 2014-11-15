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
    -- * Registers
    , register
    , writeRegister
    , readRegister
    -- * CPU Flags
    , setCPSR
    , readCPSR
    , withCPSR
    , currentBankMode
    , setBankMode
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
import qualified Data.Vector.Mutable as MV
import qualified Data.Vector.Unboxed.Mutable as MUV

import           Game.GBA.Register
import           Game.GBA.Memory

data GBAContext s = GBAContext {
        _gbaRegisters :: RegisterSet s
      , _gbaMemoryMap :: MemoryMap s
    }

makeLenses ''GBAContext

-- | Creates an empty context.
makeGBAContext :: ST s (GBAContext s)
makeGBAContext = GBAContext <$> makeRegisterSet <*> makeMemoryMap

-- can be moved out if export gbaRegisters (which we should)
register :: BankMode -> RegisterID -> Lens' (GBAContext s) (Register s)
register bank reg = gbaRegisters . register' bank reg

readLens :: Lens' (GBAContext s) (STRef s a) -> GBA s a
readLens lens = view lens >>= liftBase . readSTRef

writeLens :: Lens' (GBAContext s) (STRef s a) -> a -> GBA s ()
writeLens lens val = view lens >>= liftBase . flip writeSTRef val

-- can be moved out
readCPSR :: GBA s Word32
readCPSR = readLens $ register UserMode cpsr

-- can be moved out
setCPSR :: Word32 -> GBA s ()
setCPSR = writeLens $ register UserMode cpsr

-- can be moved out
withCPSR :: (Word32 -> Word32) -> GBA s ()
withCPSR f = do
    c <- readCPSR
    setCPSR $ f c

-- can be moved out
currentBankMode :: GBA s BankMode
currentBankMode = do
    c <- readCPSR
    return . fromModeBits . fromIntegral $ shiftL c 27

-- can be moved out
setBankMode :: BankMode -> GBA s ()
setBankMode mode = do
    d <- readCPSR 
    setCPSR $ shiftL (shiftR d 5) 5 + fromIntegral (toModeBits mode)

-- can be moved out
readRegister :: RegisterID -> GBA s Word32
readRegister reg = do
    bank <- currentBankMode
    readLens $ register bank reg

-- can be moved out
writeRegister :: RegisterID -> Word32 -> GBA s ()
writeRegister reg val = do
    bank <- currentBankMode
    writeLens (register bank reg) val

-- can be moved out
writeReal :: RealAddress -> Word8 -> GBA s ()
writeReal (seg, off) val = do
    mem <- view $ gbaMemoryMap . unMemoryMap
    liftBase $ do
        segv <- MV.read mem seg
        MUV.write segv off val

-- can be moved out
readReal :: RealAddress -> GBA s Word8
readReal (seg, off) = do
    mem <- view $ gbaMemoryMap . unMemoryMap
    liftBase $ do
        segv <- MV.read mem seg
        MUV.read segv off

-- can be moved out
writeVirtual :: VirtualAddress -> Word8 -> GBA s ()
writeVirtual addr val = writeReal (virtual addr) val

-- can be moved out
readVirtual :: VirtualAddress -> GBA s Word8
readVirtual addr = readReal (virtual addr)

newtype GBA s a = GBA (ReaderT (GBAContext s) (ST s) a)
    deriving (Functor, Applicative, Monad, MonadReader (GBAContext s),
              MonadBase (ST s))

runGBA :: GBAContext s -> GBA s a -> ST s a
runGBA context (GBA gba) = runReaderT gba context

--type GBA s a = ReaderT (GBAContext s) (ST s) a
