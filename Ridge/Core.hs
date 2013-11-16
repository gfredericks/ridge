module Ridge.Core where
import Ridge.Types
import Control.Monad
import qualified Data.ByteString as B
import qualified Data.Map as M


eq :: [Object] -> Object
eq = bool . all (uncurry (==)) . (zip `ap` drop 1)

first :: Object -> Object
first (List []) = nil
first (List (a:_)) = a
first (Vector []) = nil
first (Vector (a:_)) = a

rest :: Object -> Object
rest (List []) = List []
rest (List (_:xs)) = List xs
rest (Vector []) = Vector []
rest (Vector (_:xs)) = Vector xs

get :: Object -> Object -> Object
get (Map m) k = case M.lookup k m of
  Just x -> x
  Nothing -> nil

assoc :: Object -> Object -> Object -> Object
assoc (Map m) k v = Map $ M.insert k v m

list :: [Object] -> Object
list = List

-- TODO: maybe change these to be callable via special forms and
-- define the normal things in lisp? Call it RT or something.

builtinFns :: [(B.ByteString, [Object] -> Object)]
builtinFns = [("+", (foldl (+) 0)),
              ("-", (\(x:xs) -> x - (foldl (+) 0 xs))),
              ("*", (foldl (*) 1)),
              ("=", eq),
              ("first", first . head),
              ("rest", rest . head),
              ("get", (\(m:k:[]) -> get m k)),
              ("assoc", (\(m:k:v:[]) -> assoc m k v)),
              ("list", list)]

builtins :: Env
builtins = M.fromList $ map (\(s,f)->(s, Value $ Function f)) builtinFns
