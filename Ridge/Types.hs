module Ridge.Types where
import qualified Data.EDN as EDN
import Data.Functor
import qualified Data.List as L
import qualified Data.Vector as V
import qualified Data.Map as M
import qualified Data.ByteString as B
import qualified Ridge.Vector as RV

data Object =
  Nil |
  Integer Integer |
  List [Object] |
  Vector (RV.Vector Object) |
  Map (M.Map Object Object) |
  Symbol B.ByteString |
  Keyword B.ByteString |
  String B.ByteString |
  Boolean Bool |
  Function ([Object] -> Object)

objectType :: Object -> Object
objectType Nil = Symbol "Nil"
objectType (Integer _) = Symbol "Integer"
objectType (List _) = Symbol "List"
objectType (Vector _) = Symbol "Vector"
objectType (Map _) = Symbol "Map"
objectType (Symbol _) = Symbol "Symbol"
objectType (Keyword _) = Symbol "Keyword"
objectType (String _) = Symbol "String"
objectType (Boolean _) = Symbol "Boolean"
objectType (Function _) = Symbol "Function"


class Objectifiable x where
  toObject :: x -> Object

instance Objectifiable t => Objectifiable (EDN.Tagged t) where
  toObject (EDN.NoTag x) = toObject x
  toObject (EDN.Tagged x _ _) = toObject x

instance Objectifiable EDN.Value where
  toObject (EDN.Integer x) = Integer x
  toObject (EDN.Symbol _ s) = Symbol s
  toObject (EDN.Keyword s) = Keyword s
  toObject (EDN.List xs) = List $ map toObject xs
  toObject (EDN.Vec v) = Vector $ RV.fromList $ map toObject (V.toList v)
  toObject (EDN.Map m) = Map $ M.map toObject $ M.mapKeys toObject m
  toObject (EDN.Nil) = Nil
  -- toObject (EDN.String s) = String s

instance Show Object where
  show Nil = "nil"
  show (Integer x) = show x
  show (List xs) = "(" ++ (unwords (map show xs)) ++ ")"
  show (Vector v) = "[" ++ (unwords $ RV.toList (fmap show v)) ++ "]"
  show (Map m) = "{" ++ (L.intercalate ", " (map f $ M.toList m)) ++ "}"
    where f (k,v) = (show k) ++ " " ++ (show v)
  show (Symbol s) = reverse $ tail $ reverse $ tail $ show s
  show (Keyword s) = ":" ++ (reverse $ tail $ reverse $ tail $ show s)
  show (Function _) = "#<Function>"

instance Num Object where
  (Integer a) + (Integer b) = Integer (a + b)
  (Integer a) * (Integer b) = Integer (a * b)
  (Integer a) - (Integer b) = Integer (a - b)
  negate (Integer x) = Integer (negate x)
  abs (Integer x) = Integer (abs x)
  signum (Integer x) = Integer (signum x)
  fromInteger n = Integer n

instance Integral Object where
  toInteger (Integer x) = x
instance Real Object
instance Enum Object

instance Ord Object where
  (Integer a) <= (Integer b) = (a <= b)
  (Symbol a) <= (Symbol b) = (a <= b)
  a <= b = (objectType a) <= (objectType b)
instance Eq Object where
  Nil == Nil = True
  (Integer a) == (Integer b) = (a == b)
  (List a) == (List b) = (a == b)
  (Vector a) == (Vector b) = (a == b)
  (Map a) == (Map b) = (a == b)
  (Symbol a) == (Symbol b) = (a == b)
  (Keyword a) == (Keyword b) = (a == b)
  (String a) == (String b) = (a == b)
  (Boolean a) == (Boolean b) = (a == b)
  (Function a) == (Function b) = False -- yeah?
  _ == _ = False

data EnvEntry =
  Value Object |
  Macro Object

type Env = M.Map B.ByteString EnvEntry


bool :: Bool -> Object
bool = Boolean


symbol :: B.ByteString -> Object
symbol = Symbol

nil :: Object
nil = Nil