module Demo where

import Data.Monoid

newtype Writer w a = Writer { runWriter :: (a, w) }

writer :: (a, w) -> Writer w a
writer = Writer

execWriter = snd . runWriter

instance (Monoid w) => Monad (Writer w) where
  return x = Writer (x, mempty)
  m >>= k =
    let (x,u) = runWriter m
        (y,v) = runWriter $ k x 
    in
        Writer(y, u `mappend` v)


type Shopping = Writer (Sum Integer) ()

purchase :: String -> Integer -> Shopping
purchase item cost = writer ((), Sum cost)

total :: Shopping -> Integer
total = getSum . execWriter

shopping1 :: Shopping
shopping1 = do
  purchase "Jeans" 19200
  purchase "Water"   180
  purchase "Lettuce" 328
