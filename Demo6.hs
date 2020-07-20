import Prelude hiding (lookup)
import qualified Data.List as L

class MapLike m where
    empty :: m k v
    lookup :: Ord k => k -> m k v -> Maybe v
    insert :: Ord k => k -> v -> m k v -> m k v
    delete :: Ord k => k -> m k v -> m k v
    fromList :: Ord k => [(k,v)] -> m k v
    fromList [] = empty
    fromList ((k,v):xs) = insert k v (fromList xs)

newtype ListMap k v = ListMap { getListMap :: [(k,v)] }
    deriving (Eq,Show)

instance MapLike ListMap where
    insert k v (ListMap xs) = ListMap $ helper xs
      where
        helper ((k',v'):xs') = if k' == k then (k,v) : xs' else (k',v') : helper xs'
        helper [] = [(k,v)]

    empty = ListMap []

    lookup k (ListMap xs) = helper xs
      where
        helper ((k',v):xs') = if k' == k then (Just v) else helper xs'
        helper [] = Nothing
        
    delete k (ListMap xs) = ListMap $ helper xs
      where
        helper ((k',v):xs') = if k' == k then xs' else (k',v) : helper xs'
        helper [] = []
