-- | Instructionset for THUMB mode.
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
module Game.GBA.Thumb.Instruction
    ( TInstruction(..)
    , parseT
    -- * Opcodes
    , TSROpcode(..)
    , TASOperandType(..)
    , TASOperation(..)
    , TMCASOpcode(..)
    )
where

import           Control.Applicative
import           Control.Monad.State
import           Data.Bits
import           Data.Maybe
import           Data.Word
import           Language.Literals.Binary
import           Game.GBA.Register

-- | Shift register.
data TSROpcode = TSRO_LSL -- ^ Shift left
               | TSRO_LSR -- ^ Logical shift right
               | TSRO_ASR -- ^ Arithmetic shift right (preseves sign bit)
                          -- if the opcode is 11, its actually a TAS (t2)
               deriving (Enum, Show, Read, Eq, Ord)

data TASOperandType = TASO_REG
                    | TASO_NUM
                    deriving (Enum, Show, Read, Eq, Ord)

data TASOperation = TASO_ADD
                  | TASO_SUB
                  deriving (Enum, Show, Read, Eq, Ord)

-- | Move, compare, add or subtract. Compare only modifies CPU flags.
data TMCASOpcode = TMCASO_MOV
                 | TMCASO_CMP
                 | TMCASO_ADD
                 | TMCASO_SUB
                 deriving (Enum, Show, Read, Eq, Ord)


-- | Thumb instructions.
-- This is an intermediate format - only used right before execution.
-- For this reason, we aren't compact - we use @Int@ for register ids.
data TInstruction =
      TSR -- ^ Shift register. (t1)
        TSROpcode -- ^ Opcode, one of LSL, LSR, ASR (2 bits)
        Word8 -- ^ Offset (5 bits)
        RegisterID -- ^ Source (3 bits)
        RegisterID -- ^ Destination (3 bits)
    | TAS -- ^ Add or subtract register into another. (t2)
        TASOperandType -- MSB of opcode (1 bit)
        TASOperation -- LSB of opcode (0 bit)
        Word8 -- ^ Either a register ID or a number (3 bits)
        RegisterID -- ^ Source register (3 bits)
        RegisterID -- ^ Destination register (3 bits)
    | TMCAS -- ^ Move, compare, add, or subtract. (t3)
        TMCASOpcode -- ^ Opcode, one of MOV, CMP, ADD, SUB (2 bits)
        RegisterID -- ^ Destination register
        Word8 -- Unsigned immediate (8 bits)
    deriving (Show, Read, Eq, Ord)

chop x t = shiftR (shiftL x t) t

-- | For reasons, this can't be an instance of Alternative, because its
-- just not strong enough.
--
-- This isn't a real parsing monad! Don't use for anything more than
-- naive sequential parsing!!!
newtype Parser a = Parser (State (Int, Word16) a)
    deriving (Functor, Applicative, Monad, MonadState (Int, Word16))

-- because I'm too lazy to write this as a fold, really.
-- Todo: refactor
choose :: Word16 -> [Parser (Maybe a)] -> Maybe a
choose input [] = Nothing
choose input (x:xs) = case runParser input x of
                         Just k -> Just k
                         Nothing -> choose input xs

runParser :: Word16 -> Parser (Maybe a) -> Maybe a 
runParser input (Parser st) = evalState st (15, input)

getBits :: Integral i => Int -> Parser i
getBits k = do
    (cur, input) <- get
    let move = cur - k + 1
        val = shiftR input (cur - k + 1)
    put $ (cur - k, chop input (16 - move))
    return $ fromIntegral val

require :: Integral i => Int -> i -> Parser (Maybe a) -> Parser (Maybe a)
require k b cont = do
    bits <- getBits k
    if b == bits then cont else return Nothing

requireC :: Bool -> Parser (Maybe a) -> Parser (Maybe a)
requireC b cont = do
    if b then cont else return Nothing

-- | Parses a 16-bit thumb instruction, but does not execute.
--
-- In a perfect world, we could probably encapsulate this. But the parsing of
-- instructions is too important to not justify exporting in order to test.
parseT :: Word16 -> TInstruction
parseT inst = fromJust $ choose inst
    [ parse0
    , parse1
    , parse2
    ]

parse0 :: Parser (Maybe TInstruction)
parse0 = require 3 [b|000|] $ do
    opcode <- getBits 2
    requireC (opcode /= [b|11|]) $ do
        offset <- getBits 5
        source <- getBits 3
        dest <- getBits 3
        return . Just $ TSR (toEnum opcode) offset source dest

parse1 :: Parser (Maybe TInstruction)
parse1 = require 5 [b|00011|] $ do
    isImmediate <- getBits 1
    isSub <- getBits 1
    operand <- getBits 3
    source <- getBits 3
    dest <- getBits 3
    return . Just $ TAS (toEnum isImmediate) (toEnum isSub) operand source dest

parse2 :: Parser (Maybe TInstruction)
parse2 = require 3 [b|001|] $ do
    opcode <- getBits 2
    dest <- getBits 3
    operand <- getBits 8
    return . Just $ TMCAS (toEnum opcode) dest operand