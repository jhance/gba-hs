module Game.GBA.Thumb.Execution
    (execute)
where

import           Control.Applicative
import           Control.Monad
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
                    -- set left-most n bits to 1
                    then shiftL (complement 0) (32 - n') + val'
                    else val'
    writeRegister dest val''
    writeStatus statusC $ testBit val (n' - 1)
    setZero val''
    setSign val''

-- | Gets either the register or the direct number
-- for a TAS.

-- t2
-----
-- needs refactor
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

-- t3
-----
executeT3 :: TMCASOpcode -> RegisterID -> Word32 -> GBA s ()
executeT3 TMCASO_MOV reg num = do
    writeSafeRegister reg num
    setZero num
    -- num is 8 bit unsigned - it can't have a sign bit = 1.
    writeStatus statusN False
executeT3 TMCASO_ADD reg num = do
    val <- readSafeRegister reg
    let result = val + num
        pos = not $ testBit val 31
    setZero result
    setSign result
    writeStatus statusC $ result < val
    writeStatus statusV $ pos && testBit result 31
    writeSafeRegister reg result
executeT3 opcode reg num = do -- cmp, sub
    val <- readSafeRegister reg
    let result = val - num
        neg = testBit val 31
    setZero result
    setSign result
    writeStatus statusC $ val >= num
    writeStatus statusV $ neg && not (testBit result 31)
    when (opcode ==  TMCASO_SUB) $ writeSafeRegister reg result

-- t4
-----
executeT4 :: TALUOpcode -> RegisterID -> RegisterID -> GBA s ()
executeT4 TALU_AND src dest = do
    in1 <- readSafeRegister src
    in2 <- readSafeRegister dest
    let result = in1 .&. in2
    setZero result
    setSign result
    writeSafeRegister dest result

-- | Execution of any Thumb mode instruction.
-- Please do not try to execute outside of Thumb mode.
-- There is no check.
execute :: TInstruction -> GBA s ()
execute (TSR opcode offset src dest) =
    executeT1 opcode offset src dest
execute (TAS srcType opType operand source dest) =
    executeT2 srcType opType operand source dest
execute (TMCAS opcode dest num) =
    executeT3 opcode dest num
execute (TALU opcode src dest) =
    executeT4 opcode src dest
