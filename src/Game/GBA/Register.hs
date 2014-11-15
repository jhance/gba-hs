{-# LANGUAGE RankNTypes #-}
module Game.GBA.Register
    ( RegisterSet
    , Register
    , RegisterID
    , BankMode(..)
    -- * Access to registers
    , register
    , readRegister
    , writeRegister
    -- * CPSR & Bank modes
    , readCPSR
    , setCPSR
    , withCPSR
    , currentBankMode
    , setBankMode
    )
where

import           Control.Lens
import           Data.Bits
import           Data.Word
import           Game.GBA.Monad
import           Game.GBA.RegisterSet

register :: BankMode -> RegisterID -> Lens' (GBAContext s) (Register s)
register bank reg = gbaRegisters . register' bank reg

readCPSR :: GBA s Word32
readCPSR = readLens $ register UserMode cpsr

setCPSR :: Word32 -> GBA s ()
setCPSR = writeLens $ register UserMode cpsr

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

