{-# LANGUAGE OverloadedStrings #-}
module Ridge where
import Ridge.RT
import Ridge.Types
import qualified Ridge.Vector as RV
import Control.Exception
import Control.Monad
import qualified Data.EDN as EDN
import qualified Data.Map as M
import Data.Functor
import Data.Maybe
import qualified Data.Text as T
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
evalString = evalExpr emptyEnv . fromJust . readString

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
repl = do
  List forms <- coreForms
  repl' (setupEnv emptyEnv forms)

repl' :: Env -> IO ()
repl' env = do
  putStr "ridge> "
  hFlush stdout
  line <- B.getLine
  case line of
    "quit" -> return ()
    "reset" -> repl
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

coreForms :: IO Object
coreForms = do
  code <- BL.readFile "core.clj"
  let codeWithParens = "(\n" `BL.append` code `BL.append` "\n)"
    in return $ fromJust $ readString codeWithParens

setupEnv :: Env -> [Object] -> Env
setupEnv = foldl (\env form -> updateEnv env (eval env form))

-- Evals each expression and returns the value of the last one.
-- Presumably all but the last one are defs.
evalCommands :: Env -> [Object] -> Object
evalCommands env (expr:more) =
  let result = (eval env expr)
      env' = updateEnv env result
  in if null more then let (ValueResult v) = result in v else evalCommands (updateEnv env result) more

data EvalResult =
  ValueResult Object |
  DefResult T.Text Object |
  MacroResult T.Text Object

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
evalExpr env (Vector v) = Vector (fmap (evalExpr env) v)

--
-- Special evalExpr types -- lists and symbols
--
evalExpr env (emptylist@(List [])) = emptylist
evalExpr env (List (verb:args)) =
  case verb of
    SymbolNS "RT" rtname ->
      case M.lookup rtname functions of
        Just f -> apply f $ map (evalExpr env) args
        Nothing -> error "Unknown RT function!"
    Symbol "fn" -> evalExprFn env args
    Symbol "if" -> case args of
      [testExpr, trueExpr] ->
        evalExprIf env testExpr trueExpr Nil
      [testExpr, trueExpr, falseExpr] ->
        evalExprIf env testExpr trueExpr falseExpr
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

parseArgList :: [Object] -> [Object] -> Maybe (Env -> Env)
parseArgList [] args = if null args then Just id else Nothing
parseArgList [(Symbol "&"),(Symbol restArg)] args =
  Just $ M.insert restArg (Value (List args))
parseArgList ((Symbol name):more) [] = Nothing
parseArgList ((Symbol name):more) (arg:args) = do
  func <- parseArgList more args
  return (func . (M.insert name (Value arg)))

-- I tried doing recursion at the haskell level but it hung...
fixForm = fromJust $ readString "(fn [g] ((fn [x] (g (x x))) (fn [x] (g (x x)))))"

recursabilize :: Object -> Object -> Object
recursabilize fnForm fnName =
  List [fixForm, List [(Symbol "fn"), (Vector $ RV.fromList [fnName]), fnForm]]


evalExprFn :: Env -> [Object] -> Object
evalExprFn env (recurName@(Symbol _) : fnArgs) =
  evalExpr env $ recursabilize (List ((Symbol "fn") : fnArgs)) recurName
evalExprFn env args@[(Vector arglist'), expr] =
  -- Normalize (fn [] :foo) to (fn ([] :foo))
  evalExprFn env [(List args)]
evalExprFn env clauses =
  let fns = map makeFn clauses
  in Function (\args ->
                case catMaybes (map ($ args) fns) of
                  [] -> error "Wrong number of args passed to function!"
                  (res:_) -> res)
  where makeFn (List [(Vector arglist'), expr]) args =
          let arglist = RV.toList arglist'
          in do
            envFunc <- parseArgList arglist args
            return $ evalExpr (envFunc env) expr

-----
-- if
-----

evalExprIf :: Env -> Object -> Object -> Object -> Object
evalExprIf env cond trueCase falseCase =
  let (Boolean b) = evalExpr env cond
  in if b then evalExpr env trueCase else evalExpr env falseCase
