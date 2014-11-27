-- | Instructionset for THUMB mode.
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}
module Game.GBA.Thumb.Instruction
    ( TInstruction(..)
    , parseT
    -- * Opcodes
    , T1Opcode(..)
    , T2OperandType(..)
    , T2Operation(..)
    , T3Opcode(..)
    , T4Opcode(..)
    , T5Opcode(..)
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
data T1Opcode = T1_LSL -- ^ Shift left
              | T1_LSR -- ^ Logical shift right
              | T1_ASR -- ^ Arithmetic shift right (preseves sign bit)
              deriving (Enum, Show, Read, Eq, Ord)

data T2OperandType = T2_REG
                   | T2_NUM
                   deriving (Enum, Show, Read, Eq, Ord)

data T2Operation = T2_ADD
                  | T2_SUB
                  deriving (Enum, Show, Read, Eq, Ord)

-- | Move, compare, add or subtract. Compare only modifies CPU flags.
data T3Opcode = T3_MOV
                 | T3_CMP
                 | T3_ADD
                 | T3_SUB
                 deriving (Enum, Show, Read, Eq, Ord)

-- | A whole bunch of ALU instructions.
data T4Opcode = T4_AND
                | T4_EOR
                | T4_LSL
                | T4_LSR
                | T4_ASR
                | T4_ADC
                | T4_SBC
                | T4_ROR
                | T4_TST
                | T4_NEG
                | T4_CMP
                | T4_CMN
                | T4_ORR
                | T4_MUL
                | T4_BIC
                | T4_MVN
                deriving (Enum, Show, Read, Eq, Ord)

data T5Opcode = T5_ADD
              | T5_CMP
              | T5_MOV
              deriving (Enum, Show, Read, Eq, Ord)

-- | Thumb instructions.
-- This is an intermediate format - only used right before execution.
-- For this reason, we aren't compact - we use @Int@ for register ids.
data TInstruction =
      T1 -- Shift register. (t1)
        T1Opcode -- Opcode, one of LSL, LSR, ASR (2 bits)
        {-# UNPACK #-} !Word8 -- Offset (5 bits)
        {-# UNPACK #-} !RegisterID -- Source (3 bits)
        {-# UNPACK #-} !RegisterID -- Destination (3 bits)
    | T2 -- Add or subtract register into another. (t2)
        T2OperandType -- MSB of opcode (1 bit)
        T2Operation -- LSB of opcode (0 bit)
        {-# UNPACK #-} !Word8 -- Either a register ID or a number (3 bits)
        {-# UNPACK #-} !RegisterID -- Source register (3 bits)
        {-# UNPACK #-} !RegisterID -- Destination register (3 bits)
    | T3 -- Move, compare, add, or subtract. (t3)
        T3Opcode -- Opcode, one of MOV, CMP, ADD, SUB (2 bits)
        {-# UNPACK #-} !RegisterID -- Destination register
        {-# UNPACK #-} !Word32 -- Unsigned immediate (8 bits)
    | T4 -- ALU operations
        T4Opcode -- Opcode. Many options. (4 bits)
        {-# UNPACK #-} !RegisterID -- Source register
        {-# UNPACK #-} !RegisterID -- Destination register
    | T5 -- High-register arithmetic.
        T5Opcode -- Opcode. Add/cmp/mov.
        {-# UNPACK #-} !RegisterID -- Source register
        {-# UNPACK #-} !RegisterID -- Destination register
    | T6 -- Branch-exchange (no BLX, not supported by ARM7)
        {-# UNPACK #-} !RegisterID -- Source register (contains jump loc)

    deriving (Show, Read, Eq, Ord)

chop x t = shiftR (shiftL x t) t

newtype Parser a = Parser (MaybeT (State (Int, Word16)) a)
    deriving (Functor, Applicative, Alternative, Monad, MonadPlus, MonadState (Int, Word16))

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

-- | Like @choice@, but with backtracking. More useful.
choiceTry :: [Parser a] -> Parser a
choiceTry = choice . map try

-- | Parses a 16-bit thumb instruction, but does not execute.
--
-- If it fails to parse, it will error. Really it should through an exception, though.
parseT :: Word16 -> Maybe TInstruction
parseT inst = runParser inst $ choiceTry
    [ parse1
    , parse2
    , parse3
    , parse4
    , parse5
    , parse6
    ]

parse1 :: Parser TInstruction
parse1 = do
    require 3 [b|000|] 
    opcode <- getBits 2
    guard $ opcode /= [b|11|]
    let opcode' = toEnum opcode
    offset <- getBits 5
    source <- getBits 3
    dest <- getBits 3
    let offset' = if offset == 0 && (opcode' == T1_ASR || opcode' == T1_LSR)
            then 32
            else offset
    return $ T1 opcode' offset' source dest

parse2 :: Parser TInstruction
parse2 = do
    require 5 [b|00011|]
    isImmediate <- toEnum <$> getBits 1
    isSub <- toEnum <$> getBits 1
    operand <- getBits 3
    source <- getBits 3
    dest <- getBits 3
    return $ T2 isImmediate isSub operand source dest

parse3 :: Parser TInstruction
parse3 = do
    require 3 [b|001|]
    opcode <- toEnum <$> getBits 2
    dest <- getBits 3
    operand <- getBits 8
    return $ T3 opcode dest operand

parse4 :: Parser TInstruction
parse4 = do
    require 6 [b|010000|]
    opcode <- toEnum <$> getBits 4
    source <- getBits 3
    dest <- getBits 3
    return $ T4 opcode source dest

parse5 :: Parser TInstruction
parse5 = do
    require 6 [b|010001|]
    opcode <- getBits 2
    guard $ opcode /= 3
    let opcode' = toEnum opcode
    dmsb <- getBits 1
    src <- getBits 4
    dest <- getBits 3
    return $ T5 opcode' src (dest + 8 * dmsb)

parse6 :: Parser TInstruction
parse6 = do
    require 9 [b|010001110|]
    T6 <$> getBits 4
