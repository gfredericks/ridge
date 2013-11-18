module Ridge.RT where
import Ridge.Types
import qualified Ridge.Vector as RV
import Control.Monad
import qualified Data.Text as T
import qualified Data.Map as M

eq :: Object -> Object -> Object
eq a b = Boolean (a == b)

type' :: Object -> Object
type' x = objectType x

--
-- Lists
--

listFirst :: Object -> Object
listFirst (List []) = nil
listFirst (List (x:_)) = x

listRest :: Object -> Object
listRest (List []) = nil
listRest (List (_:xs)) = List xs

listCons :: Object -> Object -> Object
listCons x xs = List (x:(unwrapList xs))
-- using unwrapList like this rather than destructuring in the arglist
-- makes it lazier since haskell doesn't need to unwrap it to determine
-- if the clause matches. or whatever?
  where unwrapList (List xs) = xs

--
-- Maps
--

mapGet :: Object -> Object -> Object
mapGet (Map m) k = case M.lookup k m of
  Just x -> x
  Nothing -> nil

mapAssoc :: Object -> Object -> Object -> Object
mapAssoc (Map m) k v = Map (M.insert k v m)

mapDissoc :: Object -> Object -> Object
mapDissoc (Map m) k = Map (M.delete k m)

--
-- Vectors
--

vectorNth :: Object -> Object -> Object
vectorNth (Vector v) (Integer i) =
  case RV.get (fromIntegral i) v of
    Just x -> x
    Nothing -> error "Vector index out of bounds!"

vectorPop :: Object -> Object
vectorPop (Vector v) = Vector $ RV.pop v

vectorConj :: Object -> Object -> Object
vectorConj (Vector v) x = Vector $ RV.conj x v

vectorCount :: Object -> Object
vectorCount (Vector v) = Integer $ fromIntegral $ RV.size v

--
-- Arithmetic
--

plus :: Object -> Object -> Object
plus a b = a + b

negate' :: Object -> Object
negate' a = negate a

times :: Object -> Object -> Object
times a b = a * b

lt :: Object -> Object -> Object
lt a b = Boolean $ a < b

--
-- Functions
--

apply' :: Object -> Object -> Object
apply' (Function f) (List args) = f args


--
-- RT Registry
--

-- Used when args can be any type
tAny :: Object
tAny = Symbol "Any"

data RTFunction =
  -- The initial Object's are the types of the args
  RTFunction1 Object (Object -> Object) |
  RTFunction2 Object Object (Object -> Object -> Object) |
  RTFunction3 Object Object Object (Object -> Object -> Object -> Object)


functions :: M.Map T.Text RTFunction
functions = M.fromList [("eq", RTFunction2 tAny tAny eq),
                        ("type", RTFunction1 tAny type'),
                        ("listFirst", RTFunction1 tList listFirst),
                        ("listRest", RTFunction1 tList listRest),
                        -- using tAny here for the second arg allows listCons
                        -- to be lazy
                        ("listCons", RTFunction2 tAny tAny listCons),
                        ("mapGet", RTFunction2 tMap tAny mapGet),
                        ("mapAssoc", RTFunction3 tMap tAny tAny mapAssoc),
                        ("mapDissoc", RTFunction2 tMap tAny mapDissoc),
                        ("vectorNth", RTFunction2 tVector tInteger vectorNth),
                        ("vectorPop", RTFunction1 tVector vectorPop),
                        ("vectorConj", RTFunction2 tVector tAny vectorConj),
                        ("vectorCount", RTFunction1 tVector vectorCount),
                        ("plus", RTFunction2 tInteger tInteger plus),
                        ("negate", RTFunction1 tInteger negate'),
                        ("times", RTFunction2 tInteger tInteger times),
                        ("lt", RTFunction2 tInteger tInteger lt),
                        ("apply", RTFunction2 tFunction tList apply')]

arity :: RTFunction -> Int
arity (RTFunction1 _ _) = 1
arity (RTFunction2 _ _ _) = 2
arity (RTFunction3 _ _ _ _) = 3

assertTypes :: [(Object, Object)] -> Object -> Object
assertTypes [] val = val
assertTypes ((x,t):pairs) val =
  if (t == tAny) || (objectType x) == t then assertTypes pairs val else error "Wrong type passed to RT function!"

apply :: RTFunction -> [Object] -> Object
apply (RTFunction1 t1 f) [x1] =
  assertTypes [(x1,t1)] (f x1)
apply (RTFunction2 t1 t2 f) [x1,x2] =
  assertTypes [(x1,t1),(x2,t2)] (f x1 x2)
apply (RTFunction3 t1 t2 t3 f) [x1,x2,x3] =
  assertTypes [(x1,t1),(x2,t2),(x3,t3)] (f x1 x2 x3)
apply _ _ = error "Wrong number of args passed to RT function!"