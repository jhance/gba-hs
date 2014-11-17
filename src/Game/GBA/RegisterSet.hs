{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
module Game.GBA.RegisterSet
    ( Register
    , RegisterSet(..)
    , RegisterID
    , register'
    , makeRegisterSet

    -- * Special registers
    , linkRegister
    , programCounter
    , stackPointer

    -- Status registers
    , StatusRegister(..)
    , StatusRegisterSet(..)
    , BankMode(..)
    , ProcessorMode(..)
    , makeStatusRegisterSet
    , fromModeBits
    , toModeBits
    )

where

import           Control.Applicative
import           Control.Monad.Reader
import           Control.Monad.ST
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
      register0 :: {-# UNPACK #-} !(Register s)
    , register1 :: {-# UNPACK #-} !(Register s)
    , register2 :: {-# UNPACK #-} !(Register s)
    , register3 :: {-# UNPACK #-} !(Register s)
    , register4 :: {-# UNPACK #-} !(Register s)
    , register5 :: {-# UNPACK #-} !(Register s)
    , register6 :: {-# UNPACK #-} !(Register s)
    , register7 :: {-# UNPACK #-} !(Register s)
    , register8 :: {-# UNPACK #-} !(Register s)
    , register9 :: {-# UNPACK #-} !(Register s)
    , register10 :: {-# UNPACK #-} !(Register s)
    , register11 :: {-# UNPACK #-} !(Register s)
    , register12 :: {-# UNPACK #-} !(Register s)
    , register13 :: {-# UNPACK #-} !(Register s)
    , register14 :: {-# UNPACK #-} !(Register s)
    , register15 :: {-# UNPACK #-} !(Register s)
    , register8fiq :: {-# UNPACK #-} !(Register s)
    , register9fiq :: {-# UNPACK #-} !(Register s)
    , register10fiq :: {-# UNPACK #-} !(Register s)
    , register11fiq :: {-# UNPACK #-} !(Register s)
    , register12fiq :: {-# UNPACK #-} !(Register s)
    , register13fiq :: {-# UNPACK #-} !(Register s)
    , register14fiq :: {-# UNPACK #-} !(Register s)
    , register13svc :: {-# UNPACK #-} !(Register s)
    , register14svc :: {-# UNPACK #-} !(Register s)
    , register13abt :: {-# UNPACK #-} !(Register s)
    , register14abt :: {-# UNPACK #-} !(Register s)
    , register13irq :: {-# UNPACK #-} !(Register s)
    , register14irq :: {-# UNPACK #-} !(Register s)
    , register13und :: {-# UNPACK #-} !(Register s)
    , register14und :: {-# UNPACK #-} !(Register s)
    }

data StatusRegisterSet s = StatusRegisterSet {
      registerCPSR :: {-# UNPACK #-} !(StatusRegister s)
    , registerSPSRfiq :: {-# UNPACK #-} !(StatusRegister s)
    , registerSPSRsvc :: {-# UNPACK #-} !(StatusRegister s)
    , registerSPSRabt :: {-# UNPACK #-} !(StatusRegister s)
    , registerSPSRirq :: {-# UNPACK #-} !(StatusRegister s)
    , registerSPSRund :: {-# UNPACK #-} !(StatusRegister s)
    }

data StatusRegister s = StatusRegister
    { statusN :: {-# UNPACK #-} !(STRef s Bool)
    , statusZ :: {-# UNPACK #-} !(STRef s Bool)
    , statusC :: {-# UNPACK #-} !(STRef s Bool)
    , statusV :: {-# UNPACK #-} !(STRef s Bool)
    , statusQ :: {-# UNPACK #-} !(STRef s Bool)
    , statusI :: {-# UNPACK #-} !(STRef s Bool)
    , statusF :: {-# UNPACK #-} !(STRef s Bool)
    , statusT :: {-# UNPACK #-} !(STRef s ProcessorMode)
    , statusB :: {-# UNPACK #-} !(STRef s BankMode)
    }

-- | Generally we probably spend most of our time in @UserMode@.
data BankMode = UserMode | FIQMode | SupervisorMode | AbortMode | IRQMode | UndefinedMode | SystemMode

-- | Processor state.
data ProcessorMode = ARM | Thumb
    deriving (Enum, Ord, Eq, Show, Read)

-- | Converts a 5-bit representation into a bank mode.
fromModeBits :: Integral i => i -> BankMode
fromModeBits [b|10000|] = UserMode
fromModeBits [b|10001|] = FIQMode
fromModeBits [b|10010|] = IRQMode
fromModeBits [b|10011|] = SupervisorMode
fromModeBits [b|10111|] = AbortMode
fromModeBits [b|11011|] = UndefinedMode
fromModeBits [b|11111|] = SystemMode
fromModeBits _ = error "fromModeBits: invalid mode"

-- | Converts a @BankMode@ into its 5-bit representation.
toModeBits :: Integral i => BankMode -> i
toModeBits UserMode = [b|10000|]
toModeBits FIQMode = [b|10001|]
toModeBits IRQMode = [b|10010|]
toModeBits SupervisorMode = [b|10011|]
toModeBits AbortMode = [b|10111|]
toModeBits UndefinedMode = [b|11011|]
toModeBits SystemMode = [b|11111|]

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
register' :: BankMode -> RegisterID -> RegisterSet s -> Register s
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

-- program pointer
register' _ 15 = register15

makeStatusRegister :: ST s (StatusRegister s)
makeStatusRegister = let k = newSTRef False in StatusRegister
    <$> k <*> k <*> k <*> k <*> k
    <*> k <*> k <*> newSTRef ARM <*> newSTRef SystemMode

makeStatusRegisterSet :: ST s (StatusRegisterSet s)
makeStatusRegisterSet = let m = makeStatusRegister in StatusRegisterSet
    <$> m <*> m <*> m <*> m <*> m
    <*> m

-- | Makes a register set, with every register initialized to zero.
--
-- This is the only way to create a register set. Normally this only
-- needs to be done by boot process or test harnesses.
makeRegisterSet :: ST s (RegisterSet s)
makeRegisterSet = let k = newSTRef 0 in RegisterSet
    <$> k <*> k <*> k <*> k <*> k -- 0
    <*> k <*> k <*> k <*> k <*> k -- 5
    <*> k <*> k <*> k <*> k <*> k -- 10
    <*> k <*> k <*> k <*> k <*> k -- 15
    <*> k <*> k <*> k <*> k <*> k -- 20
    <*> k <*> k <*> k <*> k <*> k -- 25
    <*> k