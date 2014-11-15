module Game.GBA.Thumb.Execution
    (execute)
where

import           Data.Bits
import           Data.Word
import           Game.GBA.CPUFlag
import           Game.GBA.Thumb.Instruction
import           Game.GBA.Monad
import           Game.GBA.Register

setZero :: Word32 -> GBA s ()
setZero val = setCondition CFZero $ val == 0

setSign :: Word32 -> GBA s ()
setSign val = setCondition CFSign $ testBit val 31

-- t1
-----
executeT1 :: TSROpcode -> Word8 -> RegisterID -> RegisterID -> GBA s ()
executeT1 TSRO_LSL 0 src dest = do
    val <- readRegister src
    writeRegister dest val
    setSign val
    setZero val
executeT1 _ 0 _ _ = error "invalid t1; should through exception!"
executeT1 TSRO_LSL n src dest = do
    val <- readRegister src
    let val' = shiftL val (fromIntegral n)
    writeRegister dest val'
    setCondition CFCarry $ testBit val 31
    setZero val'
    setSign val'
executeT1 TSRO_LSR n src dest = do
    val <- readRegister src
    let val' = shiftR val (fromIntegral n)
    writeRegister dest val'
    setCondition CFCarry $ testBit val (fromIntegral n - 1)
    setZero val'
    setSign val'

-- | Gets either the register or the direct number
-- for a TAS.
tasValue :: TASOperandType -> Word8 -> GBA s Word32
tasValue TASO_REG = readRegister . fromIntegral
tasValue TASO_NUM = return . fromIntegral

tasOperation :: TASOperation -> Word32 -> Word32 -> Word32
tasOperation TASO_ADD = (+)
tasOperation TASO_SUB = (-)

-- carry, zero
tasFlags :: TASOperation -> Integer -> Integer -> [(ConditionFlag, Bool)]
tasFlags = undefined

-- t2
-----
executeT2 = undefined

-- | This is a big function! And really important!!
execute :: TInstruction -> GBA s ()

-- t1
execute (TSR opcode offset src dest) = executeT1 opcode offset src dest

-- t2
execute (TAS srcType opType operand source dest) = do
    operand' <- tasValue srcType operand
    value <- readRegister source
    let result = tasOperation opType value operand'
    writeRegister dest result
    -- TODO CPU FLAGS