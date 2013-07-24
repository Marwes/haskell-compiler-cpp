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
    instructionName :: InstructionEnum,
    argument0 :: Int32,
    argument1 :: Word8,
    argument2 :: Word8
    }


instance Show Instruction where
    show (Instruction opcode arg0 arg1 arg2) =
        show opcode ++ " " ++ show arg0 ++ "," ++ show arg1 ++ "," ++ show arg2

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
            Nothing -> fail $ "Unrecognized instruction " ++ show name
            Just opcode -> return opcode
        put op
        put arg0
        put arg1
        put arg2


data InstructionEnum = NOP
                     | MOVE
                     | LOADI
                     | LOADSTR
                     | NEWOBJECT
                     | GETFIELD
                     | SETFIELD
                     | ADD
                     | SUBTRACT
                     | MULTIPLY
                     | DIVIDE
                     | REMAINDER
                     | CALL
                     deriving (Eq, Enum, Show, Read)

instructionList = enumFromTo NOP CALL
keywords = map show $ instructionList

instructionTable :: [(InstructionEnum, Char)]
instructionTable = zip instructionList $ iterate succ '\0'

instructionNames = map swap instructionTable


makeInstruction :: InstructionEnum -> Int32 -> Word8 -> Word8 -> B.ByteString
makeInstruction keyword arg0 arg1 arg2 = encode i ++ encode arg0 ++ encode arg1 ++ encode arg2
    where
        (++) = B.append
        Just i = lookup keyword instructionTable
