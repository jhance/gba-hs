-- | Instructionset for THUMB mode.
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
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
import           Control.Monad.Trans.Maybe
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

newtype Parser a = Parser (MaybeT (State (Int, Word16)) a)
    deriving (Functor, Applicative, Monad, MonadPlus, MonadState (Int, Word16))

instance Alternative Parser where 
      empty = mzero
      (<|>) = mplus

-- | Backtracking.
try :: Parser a -> Parser a
try (Parser p) = Parser $ do
    prev <- get
    flip mapMaybeT p $ \act -> act >>= \case
        Nothing -> put prev >> return Nothing
        Just k -> return (Just k)

runParser :: Word16 -> Parser a -> Maybe a 
runParser input (Parser st) = evalState (runMaybeT st) (15, input)

getBits :: Integral i => Int -> Parser i
getBits k = do
    (cur, input) <- get
    let move = cur - k + 1
        val = shiftR input (cur - k + 1)
    put $ (cur - k, chop input (16 - move))
    return $ fromIntegral val

require :: Integral i => Int -> i -> Parser ()
require k b = do
    bits <- getBits k
    guard $ b == bits

choice :: [Parser a] -> Parser a
choice = foldl (<|>) empty

choiceTry :: [Parser a] -> Parser a
choiceTry = choice . map try

-- | Parses a 16-bit thumb instruction, but does not execute.
--
-- In a perfect world, we could probably encapsulate this. But the parsing of
-- instructions is too important to not justify exporting in order to test.
parseT :: Word16 -> TInstruction
parseT inst = fromMaybe (error "Failed to parse instruction") . runParser inst $ choiceTry
    [ parse0
    , parse1
    , parse2
    ]

parse0 :: Parser TInstruction
parse0 = do
    require 3 [b|000|] 
    opcode <- getBits 2
    guard $ opcode /= [b|11|]
    offset <- getBits 5
    source <- getBits 3
    dest <- getBits 3
    return $ TSR (toEnum opcode) offset source dest

parse1 :: Parser TInstruction
parse1 = do
    require 5 [b|00011|]
    isImmediate <- getBits 1
    isSub <- getBits 1
    operand <- getBits 3
    source <- getBits 3
    dest <- getBits 3
    return $ TAS (toEnum isImmediate) (toEnum isSub) operand source dest

parse2 :: Parser TInstruction
parse2 = do
    require 3 [b|001|]
    opcode <- getBits 2
    dest <- getBits 3
    operand <- getBits 8
    return $ TMCAS (toEnum opcode) dest operand