import Control.Monad

newtype State s a = State { runState :: s -> (a,s) }

instance Monad (State s) where
  return a = State $ \st -> (a, st)
  m >>= k = State $ \st ->
    let
      (a, st') = runState m st
      m' = k a
    in
       runState m' st'

execState :: State s a -> s -> s
execState m s = snd (runState m s)

evalState :: State s a -> s -> a
evalState m s = fst (runState m s)

get :: State s s 
get = State $ \st -> (st, st) 

put :: s -> State s ()
put st = State $ \_ -> ((), st)


data Tree a = Leaf a | Fork (Tree a) a (Tree a) deriving Show

numberTree :: Tree () -> Tree Integer
numberTree tree =  (evalState $ helper tree) 1

helper :: Tree () -> State Integer (Tree Integer)
helper (Leaf _) = do
  e <- get
  put(e + 1)
  return $ Leaf e

helper (Fork l _ r) = do
  l' <- helper l
  e <- get
  put(e + 1)
  r' <- helper r
  return $ Fork l' e r'
