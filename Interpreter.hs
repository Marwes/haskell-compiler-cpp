module Interpreter (interpret) where
import qualified Data.Map as Map
import Control.Monad.State
import Expression

data Interpreter = Interpreter {
    currentClosure :: IClosure
    }

data IExpr = ExprDefault Expr
           | Primitive (Expr -> Maybe IExpr)

data IModule = IModule { 
    moduleVariables :: Map.Map String IExpr
    }

data IClosure = IClosure {
    variables :: Map.Map String IExpr,
    outer :: IClosure
    }
    | ITop IModule

type InterpreterState = State Interpreter

primitives :: Map.Map String (Expr -> Maybe IExpr)
primitives = Map.fromList [("+", add')]

(.+) :: Expr -> Expr -> Maybe IExpr
(.+) (Literal (Integer l)) (Literal (Integer r)) = Just $ ExprDefault $ Literal $ Integer $ l + r
(.+) (Literal (Float l)) (Literal (Float r)) = Just $ ExprDefault $ Literal $ Float $ l + r
(.+) _ _ = Nothing

add' :: Expr -> Maybe IExpr
add' l = Just $ Primitive $ (l.+)
add' l = Just $ Primitive $ (l.+)

lookupIExpr name (ITop (IModule vars)) = Map.lookup name vars
lookupIExpr name (IClosure vars outer) = case Map.lookup name vars of
                                            Just expr -> Just expr
                                            Nothing -> lookupIExpr name outer

getVariable :: String -> InterpreterState (Maybe IExpr)
getVariable name = gets (lookupIExpr name . currentClosure)

interpret :: Expr -> State Interpreter IExpr
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
            Just x -> return x
            Nothing -> fail "Failed in primitive function"
        ExprDefault x -> case x of
            Lambda bind expr -> do
                closure <- gets currentClosure
                let newClosure = IClosure (Map.singleton bind (ExprDefault rhs)) closure 
                return $ evalState (interpret expr) (Interpreter newClosure)
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

testExpr = applyArgs (Var "+") [Literal $ Integer 2, Literal $ Integer 3]
runExpr expr = case evalState (interpret expr) st of
    ExprDefault expr -> expr
    _ -> error "Expression returned primitive"
    where
        st = Interpreter $ ITop (IModule Map.empty)

parseAndRun xs = do
    expr <- parseExpr xs
    return $  runExpr expr
