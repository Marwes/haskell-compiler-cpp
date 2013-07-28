{-# LANGUAGE BangPatterns #-}
module Interpreter (interpret) where
import qualified Data.Map as Map
import Control.Monad.Identity
import Control.Monad.State
import Control.Monad.Error
import Expression

data Interpreter = Interpreter {
    currentClosure :: IClosure
    }

data IExpr = ExprDefault Expr
           | Primitive (Expr -> ErrorMsg IExpr)

data IModule = IModule { 
    moduleVariables :: Map.Map String IExpr
    }

data IClosure = IClosure {
    variables :: Map.Map String IExpr,
    outer :: IClosure
    }
    | ITop IModule

type ErrorMsg = Either String
type InterpreterState = StateT Interpreter (ErrorT String Identity)

primitives :: Map.Map String (Expr -> ErrorMsg IExpr)
primitives = Map.fromList [("negate", negate'), ("+", op (+) (+)), ("-", op (-) (-)), ("*", op (*) (*))]

makeOp fi _ (Literal (Integer l)) (Literal (Integer r)) = Right $ ExprDefault $ Literal $ Integer $ fi l r
makeOp _ fd (Literal (Float l)) (Literal (Float r)) = Right $ ExprDefault $ Literal $ Float $ fd l r
makeOp _ _ l r = Left $ show l ++ " + " ++ show r ++ " is not valid"

op fi fd l = Right $ Primitive $ op l 
    where
        op = makeOp fi fd



negate' :: Expr -> ErrorMsg IExpr
negate' (Literal (Integer i)) = Right $ ExprDefault $ Literal $ Integer (-i)
negate' (Literal (Float i)) = Right $ ExprDefault $ Literal $ Float (-i)
netate' _ = Left "Can only negate numbers"

lookupIExpr name (ITop (IModule vars)) = Map.lookup name vars
lookupIExpr name (IClosure vars outer) = case Map.lookup name vars of
                                            Just expr -> Just expr
                                            Nothing -> lookupIExpr name outer

maybeError  _ (Just x) = return x
maybeError msg Nothing = throwError msg


getVariable :: String -> InterpreterState (Maybe IExpr)
getVariable name = gets (lookupIExpr name . currentClosure)

tryExprApply (Lambda bind expr) rhs = do
    closure <- gets currentClosure
    let newClosure = IClosure (Map.singleton bind (ExprDefault rhs)) closure 
    case runExpr (expr) (Interpreter newClosure) of
        Left msg -> throwError msg
        Right expr -> return $ ExprDefault expr
tryExprApply _ _ = throwError "Not a function"

toExpr :: String -> IExpr -> InterpreterState Expr
toExpr msg (Primitive _) = throwError msg
toExpr _ (ExprDefault expr) = return expr


interpret :: Expr -> InterpreterState IExpr
interpret v@(Literal _) = return $ ExprDefault v
interpret (Var name) = do
    maybeIdent <- getVariable name
    case maybeIdent of
        Nothing -> do
            let msg = "Could not find primitive " ++ name 
                tryPrimitive = maybeError msg $ Map.lookup name primitives
            liftM Primitive tryPrimitive
        Just expr -> return expr
interpret (Apply lhs rhs) = do
    func <- interpret lhs
    case func of
        Primitive prim -> do
            r <- interpret rhs >>= toExpr "Primitive functions cannot take primitives"
            case prim r of
                Right expr -> return expr
                Left msg -> throwError msg
        ExprDefault x -> tryExprApply x rhs
interpret (Case expr choices) = do
    result <- interpret expr >>= toExpr "Primitives can't be patternmatched"
    case match result choices of
        x:_ -> return $ ExprDefault x
        [] -> throwError "Patterns exhausted"
interpret x = return $ ExprDefault x


match :: Expr -> [(Pattern, Expr)] -> [Expr]
match expr patterns = map snd $ filter (matchPattern expr . fst) patterns

matchPattern :: Expr -> Pattern -> Bool
matchPattern expr (Pattern dataCtorName restPatterns) = case expr of
    Apply (Var maybeCtor) rest -> dataCtorName == maybeCtor && all (matchPattern rest) restPatterns
    _ -> False
matchPattern expr (Binding (Identifier _)) = True
matchPattern expr (PatternLiteral literal) = Literal literal == expr

add = Lambda "x" $ Lambda "y" undefined

defaultModule = IModule (Map.empty)

defaultInterpreter = Interpreter $ ITop (IModule Map.empty)


testExpr = applyArgs (Var "+") [Literal $ Integer 2, Literal $ Integer 3]
runExpr :: Expr -> Interpreter -> Either String Expr
runExpr expr st = runIdentity $ do
    result <- runErrorT $ runStateT (interpret expr) st
    case result of
        Left msg -> return $ Left msg
        Right (iexpr, _) -> case iexpr of
            ExprDefault expr -> return $ Right expr
            _ -> error "Expression returned primitive"

parseAndRun xs = do
    expr <- case parseExpr xs of
        Left err -> Left $ show err
        Right expr -> Right expr
    runExpr expr defaultInterpreter
