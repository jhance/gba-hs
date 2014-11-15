{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
module Game.GBA.Monad
    ( GBA
    -- * General context lens operations
    , readLens
    , writeLens
    -- * Registers
    , register
    , writeRegister
    , readRegister
    -- * Stuff that belongs in Game.GBA.CPUFlags
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
import           GHC.Generics

import           Game.GBA.Register

data GBAContext s = GBAContext {
        _gbaRegisters :: RegisterSet s
    }

makeLenses ''GBAContext

register :: BankMode -> RegisterID -> Lens' (GBAContext s) (Register s)
register bank reg = gbaRegisters . register' bank reg

readLens :: Lens' (GBAContext s) (STRef s a) -> GBA s a
readLens lens = view lens >>= liftBase . readSTRef

writeLens :: Lens' (GBAContext s) (STRef s a) -> a -> GBA s ()
writeLens lens val = view lens >>= liftBase . flip writeSTRef val

-- | Small optimization over @readRegister@. Also needed
-- in order to get the CPSR in order to obtain the bank
-- mode, without having the bank mode already.
readCPSR :: GBA s Word32
readCPSR = readLens $ register UserMode cpsr

-- | Small optimization over @writeRegister@.
setCPSR :: Word32 -> GBA s ()
setCPSR = writeLens $ register UserMode cpsr

-- | Quickly update the CPSR (useful for setting NZCVQ in particular)
withCPSR :: (Word32 -> Word32) -> GBA s ()
withCPSR f = do
    c <- readCPSR
    setCPSR $ f c

currentBankMode :: GBA s BankMode
currentBankMode = do
    c <- readCPSR
    return . fromModeBits . fromIntegral $ shiftL c 27

setBankMode :: BankMode -> GBA s ()
setBankMode mode = do
    d <- readCPSR 
    setCPSR $ shiftL (shiftR d 5) 5 + fromIntegral (toModeBits mode)

readRegister :: RegisterID -> GBA s Word32
readRegister reg = do
    bank <- currentBankMode
    readLens $ register bank reg

writeRegister :: RegisterID -> Word32 -> GBA s ()
writeRegister reg val = do
    bank <- currentBankMode
    writeLens (register bank reg) val

newtype GBA s a = GBA (ReaderT (GBAContext s) (ST s) a)
    deriving (Functor, Applicative, Monad, MonadReader (GBAContext s),
              MonadBase (ST s))

--type GBA s a = ReaderT (GBAContext s) (ST s) a
