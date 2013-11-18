{-# LANGUAGE OverloadedStrings #-}
module Ridge where
import Ridge.Eval
import Ridge.Reader
import Ridge.Types
import Control.Exception
import qualified Data.Map as M
import Data.Functor
import Data.Maybe
import qualified Data.Text as T
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import System.IO


call :: Object -> [Object] -> Object
call (Function f) = f

evalString :: BL.ByteString -> Object
evalString = evalExpr emptyEnv . fromJust . readString

setupEnv :: Env -> [Object] -> Env
setupEnv = foldl (\env form -> updateEnv env (eval env form))

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
