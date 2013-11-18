-- Wraping Data.IntMap to give something a little bit like clojure's
-- vectors.
module Ridge.Vector where
import Data.Functor
import qualified Data.IntMap as IM

-- Given that IntMaps know their size I think we could probably do this
-- with a newtype instead of a record...
data Vector a = Vector {size :: Int,
                        entries :: IM.IntMap a}
                deriving (Show, Eq)

empty :: Vector a
empty = Vector {size = 0, entries = IM.empty}

assoc :: Int -> a -> (Vector a) -> (Vector a)
assoc i x (Vector {size = size, entries = entries}) =
  if i == size then
    Vector {size = size + 1, entries = IM.insert i x entries}
  else if (i >= 0 && i < size) then
    Vector {size = size, entries = IM.insert i x entries}
  else
    error "bad assoc"

get :: Int -> (Vector a) -> Maybe a
get i (Vector {size = size, entries = entries}) =
  if i < 0 || i >= size then
    Nothing
  else
    IM.lookup i entries

conj :: a -> (Vector a) -> (Vector a)
conj x v = assoc (size v) x v

pop :: (Vector a) -> (Vector a)
pop Vector {size = size, entries = entries} =
  if size == 0 then
    error "Can't pop empty vector!"
  else
    Vector {size = size - 1, entries = IM.deleteMax entries}

fromList :: [a] -> Vector a
fromList = foldl (\v x -> conj x v) empty

toList :: Vector a -> [a]
-- elems returns in sorted order so this works
toList (Vector {entries = entries}) = IM.elems entries

instance Functor Vector where
  fmap f (Vector {size = size, entries = entries}) =
    Vector {size = size, entries = IM.map f entries}
