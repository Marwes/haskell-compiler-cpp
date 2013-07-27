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
    
toArgList :: Expr -> [Expr]
toArgList = reverse . toArgList'
    where
        toArgList' (Apply (Var _) expr) = [expr]
        toArgList' (Apply lhs rhs) = rhs : toArgList' lhs
        toArgList' _ = error "Not a chained function application"

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
compileExpr closure (Apply (Apply (Var name) lhs) rhs) =
    case arithInstruction name of
        Just instructionName -> do
            args <- liftM2 (++) (compileExpr closure lhs) (compileExpr closure rhs)
            return $ instruction instructionName 0 : args
        Nothing -> compileCall closure name [lhs,rhs]
compileExpr closure expr@(Apply (Var name) _) = compileCall closure name (toArgList expr)
compileExpr _ _ = undefined

compileLiteral :: Closure -> Literal -> CompileState [Instruction]
compileLiteral _ (Integer i) = return [instruction LOADI (fromIntegral i)]
compileLiteral _ (Float f) = return [FloatInstruction LOADF f]
compileLiteral _ (StringLiteral str) = do
    index <- addData (StringData str)
    return [instruction LOADSTR index]

compileFunction :: Closure -> CompileState ()
compileFunction closure = do
    let FunctionDefinition _ _ expr = function closure
    instructions <- compileExpr closure expr
    return ()
    
