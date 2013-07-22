module Assembler where
import Text.Parsec.ByteString.Lazy
import Text.ParserCombinators.Parsec hiding (GenParser, try)
import Text.Parsec.Prim (try)
import Data.Word
import Data.Int (Int32)
import Data.Binary
import Data.Tuple (swap)
import qualified Data.ByteString.Lazy.Char8 as B
import System.Environment (getArgs)
import System.IO (IOMode(..), withFile)

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

instructions :: [(String, Char)]
instructions = zip keywords $ iterate succ '\0'

instructionNames = map swap instructions


makeInstruction :: String -> Int32 -> Word8 -> Word8 -> B.ByteString
makeInstruction keyword arg0 arg1 arg2 = encode i ++ encode arg0 ++ encode arg1 ++ encode arg2
    where
        (++) = B.append
        Just i = lookup keyword instructions

bString :: String -> GenParser Char st B.ByteString
bString xs = string xs >>= return . B.pack

eol :: GenParser Char st B.ByteString
eol =   try (bString "\n\r")
    <|> try (bString "\r\n")
    <|> bString "\n"
    <|> bString "\r"

matchOne :: [String] -> GenParser Char st String
matchOne [] = fail "Could not find a match"
matchOne (x:xs) = string x <|> matchOne xs

instruction :: GenParser Char st B.ByteString
instruction = do
    keyword <- matchOne keywords
    many1 space
    numbers <- sepBy1 (many1 digit) (char ',')
    let i = case numbers of
            [] -> error ""
            [x] -> makeInstruction keyword (read x) 0 0
            [x,y] -> makeInstruction keyword (read x) (read y) 0
            (x:y:z:_) -> makeInstruction keyword (read x) (read y) (read z)
    return i

assemblyFile = do
    lines <- sepEndBy instruction eol
    eof
    return $ B.concat lines

showInt32 :: B.ByteString -> String
showInt32 x = show (decode x :: Int32)

showWord8 :: B.ByteString -> String
showWord8 x = show (decode x :: Word8)

binaryToAssembly = do
    op <- anyChar
    arg0 <- count 4 anyChar
    arg1 <- anyChar
    arg2 <- anyChar
    let Just opcode = lookup op instructionNames 
        result = opcode ++ " " ++ showInt32 (B.pack arg0) ++ "," ++ showWord8 (B.pack [arg1]) ++ "," ++ showWord8 (B.pack [arg2])
    return result
    

disassemble ::  GenParser Char st B.ByteString
disassemble = do
    result <- many binaryToAssembly
    return $ B.pack $ unlines result

run f filename outFile = do
    withFile filename ReadMode $ \handle -> do
        contents <- B.hGetContents handle
        case parse f filename contents of
            Left err -> putStrLn $ show err
            Right binary -> B.writeFile outFile binary

main = do
    x:xs <- getArgs
    let filename = if x == "-d" then head xs else x
    let baseFilename = takeWhile (/='.') filename
    if x == "-d" then
        run disassemble filename (baseFilename ++ ".disasm")
    else
        run assemblyFile filename (baseFilename ++ ".asm")

