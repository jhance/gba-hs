module Game.GBA.Thumb.Execution
    (execute)
where

import           Control.Applicative
import           Data.Bits
import           Data.Word
import           Game.GBA.Thumb.Instruction
import           Game.GBA.Monad
import           Game.GBA.Register

setZero :: Word32 -> GBA s ()
setZero val = writeStatus statusZ $ val == 0

setSign :: Word32 -> GBA s ()
setSign val = writeStatus statusN $ testBit val 31

-- t1
-----
executeT1 :: TSROpcode -> Word8 -> RegisterID -> RegisterID -> GBA s ()
executeT1 TSRO_LSL 0 src dest = do
    val <- readSafeRegister src
    writeSafeRegister dest val
    setSign val
    setZero val

executeT1 _ 0 _ _ = error "invalid t1; should through exception!"
executeT1 TSRO_LSL n src dest = do
    val <- readSafeRegister src
    let val' = shiftL val (fromIntegral n)
    writeSafeRegister dest val'
    writeStatus statusC $ testBit val 31
    setZero val'
    setSign val'
executeT1 TSRO_LSR n src dest = do
    val <- readRegister src
    let val' = shiftR val (fromIntegral n)
    writeRegister dest val'
    writeStatus statusC $ testBit val (fromIntegral n - 1)
    setZero val'
    setSign val'
executeT1 TSRO_ASR n src dest = do
    let n' = fromIntegral n
    val <- readRegister src
    let val' = shiftR val n'
        val'' = if testBit val 31
                    then shiftL (complement 0) (32 - n') + val'
                    else val'
    writeRegister dest val''
    writeStatus statusC $ testBit val (n' - 1)
    setZero val''
    setSign val''

-- | Gets either the register or the direct number
-- for a TAS.

tasOperation :: TASOperation -> Word32 -> Word32 -> Word32
tasOperation TASO_ADD = (+)
tasOperation TASO_SUB = (-)

-- t2
-----
executeT2 :: TASOperandType -> TASOperation -> Word8
          -> RegisterID -> RegisterID -> GBA s ()
executeT2 otype atype operand src dest = do
    operand' <- tasConvert atype <$> tasValue otype operand
    val <- readRegister src
    let sameSign = testBit val 31 == testBit operand' 31
        result = val + operand'
    writeRegister dest $ val + operand'
    setZero result
    setSign result
    writeStatus statusC $ result < val
    writeStatus statusV $ sameSign && testBit val 31 /= testBit result 31

tasValue :: TASOperandType -> Word8 -> GBA s Word32
tasValue TASO_REG = readRegister . fromIntegral
tasValue TASO_NUM = return . fromIntegral

tasConvert :: TASOperation -> Word32 -> Word32
tasConvert TASO_ADD x = x
tasConvert TASO_SUB x = complement x + 1

-- | Execution of any Thumb mode instruction.
-- Please do not try to execute outside of Thumb mode.
-- There is no check.
execute :: TInstruction -> GBA s ()
execute (TSR opcode offset src dest) =
    executeT1 opcode offset src dest
execute (TAS srcType opType operand source dest) =
    executeT2 srcType opType operand source dest