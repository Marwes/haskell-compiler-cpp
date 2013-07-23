module Disassemble (disassemble) where
import System.IO (IOMode(..), withFile)
import qualified Data.ByteString.Lazy as B
import Data.Int
import Data.Binary
import Data.Binary.Get
import Instruction

getAll :: Binary a => Get [a]
getAll = do
    empty <- isEmpty
    if empty
    then return []
    else do
        x <- get
        xs <- getAll
        return (x:xs)
 
getAssembly :: Get String
getAssembly = do
    entrypoint <- get :: Get Int32
    instructions <- getAll :: Get [Instruction]
    let (before, after) = splitAt (fromIntegral entrypoint `div` 4) (map show instructions)
    return $ unlines (before ++ [".entrypoint"] ++ after)

disassemble :: String -> String -> IO ()
disassemble filename outName = do
    withFile filename ReadMode $ \handle -> do
        contents <- B.hGetContents handle
        case runGetOrFail getAssembly contents of
            Left (_,_,msg) -> fail msg
            Right (_,_,str) -> writeFile outName str
