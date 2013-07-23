{-# LANGUAGE DeriveGeneric #-}
module Instruction where
import GHC.Generics (Generic)
import qualified Data.ByteString.Lazy as B
import Data.Int
import Data.Binary
import Data.Tuple (swap)
import qualified Data.Map as Map

data Assembly = Assembly {
    entrypoint :: Int32,
    methods :: Map.Map String Int32,
    instructions :: [Instruction] }
    deriving (Generic)

instance Binary Assembly

data Instruction = Instruction {
    instructionName :: String,
    argument0 :: Int32,
    argument1 :: Word8,
    argument2 :: Word8
    }


instance Show Instruction where
    show (Instruction opcode arg0 arg1 arg2) =
        opcode ++ " " ++ show arg0 ++ "," ++ show arg1 ++ "," ++ show arg2

instance Binary Instruction where
    get = do
        op <- get
        arg0 <- get
        arg1 <- getWord8
        arg2 <- getWord8
        name <- case lookup op instructionNames of
            Nothing -> fail $ "Unrecognized instruction " ++ [op]
            Just opcode -> return  opcode
        return $ Instruction name arg0 arg1 arg2
    put (Instruction name arg0 arg1 arg2) = do
        op <- case lookup name instructionTable of
            Nothing -> fail $ "Unrecognized instruction " ++ name
            Just opcode -> return opcode
        put op
        put arg0
        put arg1
        put arg2

keywords = [
    "NOP",
    "MOVE",
    "LOADI",
    "NEWOBJECT",
    "GETFIELD",
    "SETFIELD",
    "ADD",
    "SUBTRACT",
    "MULTIPLY",
    "DIVIDE",
    "REMAINDER",
    "CALL"]

instructionTable :: [(String, Char)]
instructionTable = zip keywords $ iterate succ '\0'

instructionNames = map swap instructionTable


makeInstruction :: String -> Int32 -> Word8 -> Word8 -> B.ByteString
makeInstruction keyword arg0 arg1 arg2 = encode i ++ encode arg0 ++ encode arg1 ++ encode arg2
    where
        (++) = B.append
        Just i = lookup keyword instructionTable
