{-# LANGUAGE QuasiQuotes #-}
module Test.GBA.Thumb.T2
    (tests)
where

import Control.Applicative
import Data.Bits
import Data.Word
import Language.Literals.Binary

import Test.HUnit
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)

import Game.GBA.Monad
import Game.GBA.Register
import Game.GBA.Thumb.Execution
import Game.GBA.Thumb.Instruction

import Test.GBA.Common

getT2Result :: RegisterID -> GBA s (Word32, Bool, Bool, Bool, Bool)
getT2Result reg = (,,,,)
    <$> readRegister reg
    <*> readStatus statusC
    <*> readStatus statusZ
    <*> readStatus statusN
    <*> readStatus statusV

tests :: TestTree
tests = testGroup "[t2] add/sub literal or register"
    [ testGroup "[t2.0] parser"
        [ tasParser1
        , tasParser2
        , tasParser3
        , tasParser4
        ]
    , testGroup "[t2.1] add"
        [ tasAdd1
        , tasAdd2
        , tasAdd3
        , tasAdd4
        ]
    , testGroup "[t2.2] sub"
        [ tasSub1
        ]
    ]
tasParser1 :: TestTree
tasParser1 = testCase "add register" $ parseT [b|00011 00 010 111 001|]
    @?= Just (T2 T2_REG T2_ADD [b|010|] [b|111|] [b|001|])

tasParser2 :: TestTree
tasParser2 = testCase "subtract register" $ parseT [b|00011 01 111 000 101|]
    @?= Just (T2 T2_REG T2_SUB [b|111|] [b|000|] [b|101|])

tasParser3 :: TestTree
tasParser3 = testCase "add immediate" $ parseT [b|00011 10 001 000 010|]
    @?= Just (T2 T2_NUM T2_ADD [b|001|] [b|000|] [b|010|])

tasParser4 :: TestTree
tasParser4 = testCase "subtract immediate" $ parseT [b|00011 11 010 111 011|]
    @?= Just (T2 T2_NUM T2_SUB [b|010|] [b|111|] [b|011|])

-- t2.1
-------
tasAdd1 :: TestTree
tasAdd1 = testCase "tas add simple immediate" $ do
    (dest, c, z, n, v) <- runTest $ do
        writeRegister [b|000|] $ 5
        execute (T2 T2_NUM T2_ADD 4 [b|000|] [b|001|])
        getT2Result [b|001|]
    dest @?= 9
    c @?= False
    z @?= False
    n @?= False
    v @?= False

tasAdd2 :: TestTree
tasAdd2 = testCase "tas add simple register" $ do
    (dest, c, z, n, v) <- runTest $ do
        writeRegister [b|000|] $ 5
        writeRegister [b|010|] $ 4
        execute (T2 T2_REG T2_ADD [b|010|] [b|000|] [b|001|])
        getT2Result [b|001|]
    dest @?= 9
    c @?= False
    z @?= False
    n @?= False
    v @?= False

tasAdd3 :: TestTree
tasAdd3 = testCase "tas add simple register, overflow/zero" $ do
    (dest, c, z, n, v) <- runTest $ do
        writeRegister [b|000|] $ 2^31
        writeRegister [b|010|] $ 2^31
        execute (T2 T2_REG T2_ADD [b|010|] [b|000|] [b|001|])
        getT2Result [b|001|]
    dest @?= 0
    c @?= True
    z @?= True
    n @?= False
    v @?= True

tasAdd4 :: TestTree
tasAdd4 = testCase "tas add simple register, nonsigned overflow" $ do
    (dest, c, z, n, v) <- runTest $ do
        writeRegister [b|000|] $ complement 0
        writeRegister [b|010|] $ complement 0 - 2^31
        execute (T2 T2_REG T2_ADD [b|010|] [b|000|] [b|001|])
        getT2Result [b|001|]
    --dest @?= 0
    c @?= True
    z @?= False
    n @?= False
    v @?= False

-- t2.2
-------
tasSub1 :: TestTree
tasSub1 = testCase "tas sub simple register" $ do
    (dest, c, z, n, v) <- runTest $ do
        writeRegister [b|000|] 5
        writeRegister [b|010|] 3
        execute (T2 T2_REG T2_SUB [b|010|] [b|000|] [b|001|])
        getT2Result [b|001|]
    dest @?= 2
    c @?= True -- Is this right?!
    z @?= False
    n @?= False
    v @?= False
