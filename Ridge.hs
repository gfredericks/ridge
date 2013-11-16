{-# LANGUAGE OverloadedStrings #-}
module Ridge where
import Ridge.Core
import Ridge.Types
import Control.Exception
import Control.Monad
import qualified Data.EDN as EDN
import qualified Data.Map as M
import Data.Maybe
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import System.IO

-- TODO:
--   - repl *1 *2 *3
--   - use Text instead of ByteString?

main = do
  line <- B.getLine
  putStrLn $ show $ evalString $ BL.fromStrict line

call :: Object -> [Object] -> Object
call (Function f) = f

readString :: BL.ByteString -> Maybe Object
readString = EDN.parseMaybe >=> (return . toObject)

evalString :: BL.ByteString -> Object
evalString = evalExpr builtins . fromJust . readString

-- Maybe throw if the entry is already present?
updateEnv :: Env -> EvalResult -> Env
updateEnv env (ValueResult v) = env
updateEnv env (DefResult name value) =
  M.insert name (Value value) env
updateEnv env (MacroResult name value) =
  M.insert name (Macro value) env

-- Sets *1, *2, *3 in the environment, given the next *1
updateStars :: Env -> Object -> Env
updateStars env starOne =
  foldl func env [("*1", Just (Value starOne)),
                  ("*2", M.lookup "*1" env),
                  ("*3", M.lookup "*2" env)]
  where func env' (name, mvalue) =
          case mvalue of
            Just v -> M.insert name v env'
            Nothing -> env'


repl :: IO ()
repl = repl' builtins

repl' :: Env -> IO ()
repl' env = do
  putStr "ridge> "
  hFlush stdout
  line <- B.getLine
  case line of
    "quit" -> return ()
    _else ->
      case readString (BL.fromStrict line) of
        Just val ->
          handle errorHandler $ resultHandler $ eval env val
        Nothing -> do
          putStrLn "**PARSE ERROR**"
          repl' env
  where errorHandler (SomeException e) = do
          putStrLn ("ERROR: " ++ (show e))
          putStrLn "(Type \"quit\" to quit)"
          repl' env
        resultHandler result = do
          resultPrinter result
          repl' $ let env' = updateEnv env result in case result of
            ValueResult v -> updateStars env' v
            _ -> env'
        resultPrinter x = case x of
          ValueResult v -> do
            print v
          DefResult name value -> do
            putStrLn ("defined ")
          MacroResult name value -> do
            putStrLn ("macro defined ")




-- Evals each expression and returns the value of the last one.
-- Presumably all but the last one are defs.
evalCommands :: Env -> [Object] -> Object
evalCommands env (expr:more) =
  let result = (eval env expr)
      env' = updateEnv env result
  in if null more then let (ValueResult v) = result in v else evalCommands (updateEnv env result) more

data EvalResult =
  ValueResult Object |
  DefResult B.ByteString Object |
  MacroResult B.ByteString Object

eval :: Env -> Object -> EvalResult
eval env (List [(Symbol "def"), (Symbol name), expr]) =
  DefResult name $ evalExpr env expr
eval env (List [(Symbol "defmacro"), (Symbol name), arglist, expr]) =
  MacroResult name $ evalExpr env (List [(Symbol "fn"), arglist, expr])
eval env expr = ValueResult $ evalExpr env expr


evalExpr :: Env -> Object -> Object

--
-- Things that eval to themselves
--
evalExpr env x@(Integer _) = x
evalExpr env x@(Keyword _) = x
evalExpr env x@(Boolean _) = x
evalExpr env x@(String _) = x
evalExpr env x@(Nil) = x

--
-- Collections that evalExpr their contents
--
evalExpr env (Map m) =
  Map $ M.map (evalExpr env) $ M.mapKeys (evalExpr env) m
evalExpr env (Vector xs) = Vector (map (evalExpr env) xs)

--
-- Special evalExpr types -- lists and symbols
--
evalExpr env (emptylist@(List [])) = emptylist
evalExpr env (List (verb:args)) =
  case verb of
    Symbol "fn" -> evalExprFn env (args !! 0) (args !! 1)
    Symbol "if" -> evalExprIf env (args !! 0) (args !! 1) (args !! 2)
    Symbol "quote" -> args !! 0
    Symbol s -> case M.lookup s env of
      Just (Macro (Function f)) -> evalExpr env $ f args
      _else -> evalFuncall env verb args
    _else ->
      evalFuncall env verb args
evalExpr env (Symbol s) = case M.lookup s env of
  Just (Value x) -> x
  Just (Macro _) -> error "Can't take value of a macro!"
  Nothing -> error $ "Unable to resolve symbol " -- ++ (BL.toStrict s) ++ " in this context!"

evalFuncall :: Env -> Object -> [Object] -> Object
evalFuncall env verb args =
  case (evalExpr env $ verb) of
    Function f -> f (map (evalExpr env) args)
    x -> error $ "Not a function! " ++ (show x)


-----
-- fn
-----

-- This is slow because it makes the env lookup on each call. If we
-- had some notion of "compiling" the function, we could do that just
-- once.

evalExprFn :: Env -> Object -> Object -> Object
evalExprFn env argvec expr =
  let Vector arglist = argvec
      names = map (\ (Symbol name) -> name) arglist
  in Function (\args ->
                let env' = foldl (\env (name, value) -> M.insert name (Value value) env) env $ zip names args
                in evalExpr env' expr)

-----
-- if
-----

evalExprIf :: Env -> Object -> Object -> Object -> Object
evalExprIf env cond trueCase falseCase =
  let (Boolean b) = evalExpr env cond
  in if b then evalExpr env trueCase else evalExpr env falseCase
