{-# LANGUAGE RankNTypes #-}
module Game.GBA.Register
    ( RegisterSet
    , Register
    , RegisterID
    , StatusRegister(..)
    , BankMode(..)
    , ProcessorMode(..)
    -- * Access to registers
    , register
    , readRegister
    , writeRegister
    -- * Status registers
    , readStatus
    , writeStatus
    , currentBankMode
    )
where

import           Data.Bits
import           Data.STRef
import           Data.Word
import           Game.GBA.Monad
import           Game.GBA.RegisterSet

register :: BankMode -> RegisterID -> GBAContext s -> Register s
register bank reg = register' bank reg . gbaRegisters

readStatus :: (StatusRegister s -> STRef s a) -> GBA s a
readStatus lens = readLens $ lens . registerCPSR . gbaStatusRegisters

writeStatus :: (StatusRegister s -> STRef s a) -> a -> GBA s ()
writeStatus lens = writeLens $ lens . registerCPSR . gbaStatusRegisters

currentBankMode :: GBA s BankMode
currentBankMode = readStatus statusB

-- | TODO special case spsr/cpsr
--
-- My assumption is that we don't actually compute the bank mode
-- in the case that we don't need to do to laziness - but I'm
-- not completely sure on that! In any case, bank mode is very
-- fast to calculate now.
readRegister :: RegisterID -> GBA s Word32
readRegister reg = do
    bank <- currentBankMode
    readLens $ register bank reg

-- | TODO special case spsr/cpsr
--
-- My assumption is that we don't actually compute the bank mode
-- in the case that we don't need to do to laziness - but I'm
-- not completely sure on that! In any case, bank mode is very
-- fast to calculate now.
writeRegister :: RegisterID -> Word32 -> GBA s ()
writeRegister reg val = do
    bank <- currentBankMode
    writeLens (register bank reg) val

