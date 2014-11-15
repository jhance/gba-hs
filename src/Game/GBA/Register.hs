{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
module Game.GBA.Register
    ( Register
    , RegisterSet
    , RegisterID
    , register'

    -- * Special registers
    , cpsr
    , linkRegister
    , programCounter
    , spsr
    , stackPointer

    -- * Things that don't even belong in this module
    , BankMode(..) -- TODO Needs to get out of here.
    , fromModeBits
    , toModeBits
    )

where

import           Control.Lens
import           Control.Monad.Reader
import           Control.Monad.ST
import           Data.Bits.Lens
import           Data.Binary
import           Data.Word
import           Data.STRef
import           GHC.Generics
import           Language.Literals.Binary

-- Why are these registers @STRef@s? So we don't have to write the whole thing every
-- time. Is it worth it? Maybe not, but I don't particular care enough...
--
-- Unfortunately the fact that these registers are mutable means they will be a pain
-- to debug. But they have to live in the ST monad somehow.
type Register s = STRef s Word32

type RegisterID = Int

-- | All the registers available. This datatype is purposely closed - you must
-- use the @register@ function to get a lens to it; with this function you can
-- 17 (or 16 in user mode) registers available for a specific mode.
data RegisterSet s = RegisterSet {
      _register0 :: Register s
    , _register1 :: Register s
    , _register2 :: Register s
    , _register3 :: Register s
    , _register4 :: Register s
    , _register5 :: Register s
    , _register6 :: Register s
    , _register7 :: Register s
    , _register8 :: Register s
    , _register9 :: Register s
    , _register10 :: Register s
    , _register11 :: Register s
    , _register12 :: Register s
    , _register13 :: Register s
    , _register14 :: Register s
    , _register15 :: Register s
    , _register8fiq :: Register s
    , _register9fiq :: Register s
    , _register10fiq :: Register s
    , _register11fiq :: Register s
    , _register12fiq :: Register s
    , _register13fiq :: Register s
    , _register14fiq :: Register s
    , _register13svc :: Register s
    , _register14svc :: Register s
    , _register13abt :: Register s
    , _register14abt :: Register s
    , _register13irq :: Register s
    , _register14irq :: Register s
    , _register13und :: Register s
    , _register14und :: Register s
    , _registerCPSR :: Register s
    , _registerSPSRfiq :: Register s
    , _registerSPSRsvc :: Register s
    , _registerSPSRabt :: Register s
    , _registerSPSRirq :: Register s
    , _registerSPSRund :: Register s
    }

makeLenses ''RegisterSet

-- | Generally we probably spend most of our time in @UserMode@.
data BankMode = UserMode | FIQMode | SupervisorMode | AbortMode | IRQMode | UndefinedMode | SystemMode

-- | Converts a 5-bit representation into a bank mode.
fromModeBits :: Int -> BankMode
fromModeBits [b|10000|] = UserMode
fromModeBits [b|10001|] = FIQMode
fromModeBits [b|10010|] = IRQMode
fromModeBits [b|10011|] = SupervisorMode
fromModeBits [b|10111|] = AbortMode
fromModeBits [b|11011|] = UndefinedMode
fromModeBits [b|11111|] = SystemMode
fromModeBits _ = error "fromModeBits: invalid mode"

-- | Converts a @BankMode@ into its 5-bit representation.
toModeBits :: BankMode -> Int
toModeBits UserMode = [b|10000|]
toModeBits FIQMode = [b|10001|]
toModeBits IRQMode = [b|10010|]
toModeBits SupervisorMode = [b|10011|]
toModeBits AbortMode = [b|10111|]
toModeBits UndefinedMode = [b|11011|]
toModeBits SystemMode = [b|11111|]

-- | The CPSR register.
cpsr :: RegisterID
cpsr = 16

-- | The SPSR register.
spsr :: RegisterID
spsr = 17

stackPointer :: RegisterID
stackPointer = 13

linkRegister :: RegisterID
linkRegister = 14

programCounter :: RegisterID
programCounter = 15

-- | Theres a bit of nastiness where there is no register for
-- SPSR in UserMode mode. My hope is that this never gets called,
-- as it shouldn't even get called. Therefore, this does
-- not return a @Maybe@ and is as such not total.
--
-- This is an insane amount of boilerplate, but I'm actually
-- not sure what a better way to do this is. Luckily it is
-- all encapsulated within here.
register' :: BankMode -> RegisterID -> Lens' (RegisterSet s) (Register s)
register' _ 0 = register0
register' _ 1 = register1
register' _ 2 = register2
register' _ 3 = register3
register' _ 4 = register4
register' _ 5 = register5
register' _ 6 = register6
register' _ 7 = register7

register' FIQMode 8 = register8fiq
register' FIQMode 9 = register9fiq
register' FIQMode 10 = register10fiq
register' FIQMode 11 = register11fiq
register' FIQMode 12 = register12fiq
register' _ 8 = register8
register' _ 9 = register9
register' _ 10 = register10
register' _ 11 = register11
register' _ 12 = register12

register' UserMode 13 = register13
register' UserMode 14 = register14
register' SystemMode 13 = register13
register' SystemMode 14 = register14
register' FIQMode 13 = register13fiq
register' FIQMode 14 = register14fiq
register' SupervisorMode 13 = register13svc
register' SupervisorMode 14 = register14svc
register' AbortMode 13 = register13abt
register' AbortMode 14 = register14abt
register' IRQMode 13 = register13irq
register' IRQMode 14 = register14irq
register' UndefinedMode 13 = register13und
register' UndefinedMode 14 = register14und

register' _ 15 = register15

register' _ 16 = registerCPSR

register' UserMode 17 = error "Attempted to access SPSR register in User mode"
register' SystemMode 17 = error "Attempted to access SPSR register in System mode"
register' FIQMode 17 = registerSPSRfiq
register' SupervisorMode 17 = registerSPSRsvc
register' AbortMode 17 = registerSPSRabt
register' IRQMode 17 = registerSPSRirq
register' UndefinedMode 17 = registerSPSRund