module Demo5 where

import Data.Monoid

newtype Xor = Xor { getXor :: Bool }
  deriving (Eq,Show)

xor :: Bool -> Bool -> Bool
xor a b = not (a == b)

instance Semigroup Xor where
  (Xor a) <> (Xor b) = Xor (a `xor` b)

instance Monoid Xor where
  mempty = Xor False

newtype Maybe' a = Maybe' { getMaybe :: Maybe a }
    deriving (Eq,Show)


instance Semigroup a => Semigroup (Maybe' a) where
  (Maybe' Nothing) <> (Maybe' (Just b)) = (Maybe' Nothing)
  (Maybe' (Just b)) <> (Maybe' Nothing) = (Maybe' Nothing)
  (Maybe' (Just a)) <> (Maybe' (Just b)) = Maybe' $ Just (a <> b)

instance Monoid a => Monoid (Maybe' a) where
    mempty = Maybe' (Just (mempty))
