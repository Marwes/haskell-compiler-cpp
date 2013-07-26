module Compiler where
import qualified Data.Map as Map
import qualified Data.Vector as V
import Data.Int (Int32)
import Control.Monad.State
import Instruction
import Expression

data Identifier = Local Int
                | Global String
    deriving(Eq)


data Data = StringData String
          | FunctionData String
          deriving(Eq)

data Environment = Environment {
    modul :: Module,
    stack :: V.Vector String,
    dataIdentifiers :: V.Vector Data
    }

data Definition = FuncDef FunctionDefinition
                | VarDef String
                deriving (Eq)

data Closure = Closure {
    function :: FunctionDefinition,
    variables :: Map.Map String Definition,
    outer :: Closure
    }
    | Top Module
    deriving(Eq)

emptyClosure = Top (Module Map.empty Map.empty)

type CompileState = State Environment

--tries to find a identifier in the state
lookupIdentifier :: String -> CompileState (Maybe Identifier)
lookupIdentifier name = liftM2 mplus (gets local) (gets global)
    where
        --Use of mplus acts as a short circuit for Maybe
        local = liftM Local . V.elemIndex name . stack
        global env = do
            let funcs = functions $ modul env
            FunctionDefinition name _ _ <- Map.lookup name funcs
            return $ Global name

addData :: Data -> CompileState Int32
addData d = do
    env <- get
    case V.elemIndex d (dataIdentifiers env) of
        Just i -> return $ fromIntegral i
        Nothing -> do
            put $ env { dataIdentifiers = V.snoc (dataIdentifiers env) d }
            gets (fromIntegral . V.length . dataIdentifiers)
    

compileCall :: Closure -> String -> [Expr] -> CompileState [Instruction]
compileCall closure name xs = do
    maybeId <- lookupIdentifier name
    instr <- case maybeId of
        Nothing ->  fail $ "Could not find identifier " ++ name
        Just ident -> f ident
    args <- (mapM (compileExpr closure) xs)
    return $ instr ++ concat args
    where
        f (Local index) = return [instruction MOVE (fromIntegral index)]
        f (Global ident) = do
            dataIndex <- addData (FunctionData ident)
            return [instruction CALL (fromIntegral dataIndex)]
    

arithInstruction op = lookup op [("+", ADD), ("-", SUBTRACT), ("*", MULTIPLY), ("/", DIVIDE)]

compileExpr :: Closure -> Expr -> CompileState [Instruction]
compileExpr closure (Literal lit) = compileLiteral closure lit
compileExpr closure (Call name [l,r]) =
    case arithInstruction name of
        Just instructionName -> do
            args <- liftM2 (++) (compileExpr closure l) (compileExpr closure r)
            return $ instruction instructionName 0 : args
        Nothing -> compileCall closure name [l,r]
compileExpr closure (Call name args) = compileCall closure name args
compileExpr _ _ = undefined

compileLiteral :: Closure -> Literal -> CompileState [Instruction]
compileLiteral _ (Integer i) = return [instruction LOADI (fromIntegral i)]
compileLiteral _ (Float f) = return [instruction LOADF undefined]
compileLiteral _ (StringLiteral str) = do
    index <- addData (StringData str)
    return [instruction LOADSTR index]

compileFunction :: Closure -> CompileState ()
compileFunction closure = do
    let FunctionDefinition _ _ expr = function closure
    instructions <- compileExpr closure expr
    return ()
    
