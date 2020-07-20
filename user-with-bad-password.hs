import Data.List

data Reader r a = Reader { runReader :: (r -> a) }

instance Monad (Reader r) where
  return x = Reader $ \_ -> x
  m >>= k  = Reader $ \r -> runReader (k (runReader m r)) r

local :: (r -> r) -> Reader r a -> Reader r a
local f m = Reader $ \e -> runReader m (f e)

type User = String
type Password = String
type UsersTable = [(User, Password)]

ask :: Reader r r
ask = Reader id

usersWithBadPasswords :: Reader UsersTable [User]
usersWithBadPasswords = do
  e <- ask
  return $ map fst e
--(filter (\(_,p) -> p == "123456") s)
