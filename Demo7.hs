import Prelude hiding (lookup)

class MapLike m where
    empty :: m k v
    lookup :: Ord k => k -> m k v -> Maybe v
    insert :: Ord k => k -> v -> m k v -> m k v
    delete :: Ord k => k -> m k v -> m k v
    fromList :: Ord k => [(k,v)] -> m k v

newtype ArrowMap k v = ArrowMap { getArrowMap :: k -> Maybe v }

instance MapLike ArrowMap where
  empty = ArrowMap (\x -> Nothing)
  lookup x (ArrowMap fn) = ((fn) x)
  insert k v (ArrowMap fn) = (ArrowMap (\x -> if x == k then (Just v) else ((fn) x)))
  delete k (ArrowMap fn) = (ArrowMap (\x -> if x == k then Nothing else ((fn) x)))
  fromList [] = empty
  fromList ((k,v) : xs) = insert k v (fromList xs)
