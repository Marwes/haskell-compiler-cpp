{-# LANGUAGE BangPatterns #-}
module Interpreter (interpret) where
import qualified Data.Map as Map
import Control.Monad.Identity
import Control.Monad.State
import Control.Monad.Error
import Expression
import Debug.Trace

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
primitives = Map.fromList [("+", add')]

(.+) :: Expr -> Expr -> ErrorMsg IExpr
(.+) (Literal (Integer l)) (Literal (Integer r)) = Right $ ExprDefault $ Literal $ Integer $ l + r
(.+) (Literal (Float l)) (Literal (Float r)) = Right $ ExprDefault $ Literal $ Float $ l + r
(.+) l r = Left $ show l ++ " + " ++ show r ++ " is not valid"

add' :: Expr -> ErrorMsg IExpr
add' l = Right $ Primitive $ (l.+)
add' l = Right $ Primitive $ (l.+)

lookupIExpr name (ITop (IModule vars)) = Map.lookup name vars
lookupIExpr name (IClosure vars outer) = case Map.lookup name vars of
                                            Just expr -> Just expr
                                            Nothing -> lookupIExpr name outer

getVariable :: String -> InterpreterState (Maybe IExpr)
getVariable name = gets (lookupIExpr name . currentClosure)

interpret :: Expr -> InterpreterState IExpr
interpret v@(Literal _) = return $ ExprDefault v
interpret (Var name) = do
    maybeIdent <- getVariable name
    case maybeIdent of
        Nothing -> case Map.lookup name primitives of
            Nothing -> fail $ "Could not find variable " ++ name
            Just primFun -> return $ Primitive primFun
        Just expr -> return expr
interpret (Apply lhs rhs) = do
    func <- interpret lhs
    case func of
        Primitive prim -> case prim rhs of
            Right expr -> return expr
            Left msg -> fail msg
        ExprDefault x -> case x of
            Lambda bind expr -> do
                closure <- gets currentClosure
                let newClosure = IClosure (Map.singleton bind (ExprDefault rhs)) closure 
                case runExpr (expr) (Interpreter newClosure) of
                    Left msg -> throwError msg
                    Right expr -> return $ ExprDefault expr
            _ -> fail "Not a function"
interpret (Case expr choices) = do
    result <- interpret expr
    return result
interpret x = return $ ExprDefault x


match expr patterns = filter (matchPattern expr) patterns

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
