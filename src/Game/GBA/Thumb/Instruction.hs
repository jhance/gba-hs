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
    , TALUOpcode(..)
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

-- | A whole bunch of ALU instructions.
data TALUOpcode = TALU_AND
                | TALU_EOR
                | TALU_LSL
                | TALU_LSR
                | TALU_ASR
                | TALU_ADC
                | TALU_SBC
                | TALU_ROR
                | TALU_TST
                | TALU_NEG
                | TALU_CMP
                | TALU_CMN
                | TALU_ORR
                | TALU_MUL
                | TALU_BIC
                | TALU_MVN
                deriving (Enum, Show, Read, Eq, Ord)

-- | Thumb instructions.
-- This is an intermediate format - only used right before execution.
-- For this reason, we aren't compact - we use @Int@ for register ids.
data TInstruction =
      TSR -- Shift register. (t1)
        TSROpcode -- Opcode, one of LSL, LSR, ASR (2 bits)
        {-# UNPACK #-} !Word8 -- Offset (5 bits)
        {-# UNPACK #-} !RegisterID -- Source (3 bits)
        {-# UNPACK #-} !RegisterID -- Destination (3 bits)
    | TAS -- Add or subtract register into another. (t2)
        TASOperandType -- MSB of opcode (1 bit)
        TASOperation -- LSB of opcode (0 bit)
        {-# UNPACK #-} !Word8 -- Either a register ID or a number (3 bits)
        {-# UNPACK #-} !RegisterID -- Source register (3 bits)
        {-# UNPACK #-} !RegisterID -- Destination register (3 bits)
    | TMCAS -- Move, compare, add, or subtract. (t3)
        TMCASOpcode -- Opcode, one of MOV, CMP, ADD, SUB (2 bits)
        {-# UNPACK #-} !RegisterID -- Destination register
        {-# UNPACK #-} !Word8 -- Unsigned immediate (8 bits)
    | TALU
        TALUOpcode -- Opcode. Many options. (4 bits)
        {-# UNPACK #-} !RegisterID -- Source register
        {-# UNPACK #-} !RegisterID -- Destination register
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
parseT :: Word16 -> TInstruction
parseT inst = fromMaybe (error "Failed to parse instruction") . runParser inst $ choiceTry
    [ parse0
    , parse1
    , parse2
    , parse3
    ]

parse0 :: Parser TInstruction
parse0 = do
    require 3 [b|000|] 
    opcode <- getBits 2
    guard $ opcode /= [b|11|]
    let opcode' = toEnum opcode
    offset <- getBits 5
    source <- getBits 3
    dest <- getBits 3
    let offset' = if offset == 0 && (opcode' == TSRO_ASR || opcode' == TSRO_LSR)
            then 32
            else offset
    return $ TSR opcode' offset' source dest

parse1 :: Parser TInstruction
parse1 = do
    require 5 [b|00011|]
    isImmediate <- toEnum <$> getBits 1
    isSub <- toEnum <$> getBits 1
    operand <- getBits 3
    source <- getBits 3
    dest <- getBits 3
    return $ TAS isImmediate isSub operand source dest

parse2 :: Parser TInstruction
parse2 = do
    require 3 [b|001|]
    opcode <- toEnum <$> getBits 2
    dest <- getBits 3
    operand <- getBits 8
    return $ TMCAS opcode dest operand

parse3 :: Parser TInstruction
parse3 = do
    require 6 [b|010000|]
    opcode <- toEnum <$> getBits 4
    source <- getBits 3
    dest <- getBits 3
    return $ TALU opcode source dest
