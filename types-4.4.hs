import Data.List.Split
import Data.Char(isDigit)

data Error = ParsingError | IncompleteDataError | IncorrectDataError String deriving Show

data Person = Person { firstName :: String, lastName :: String, age :: Int } deriving Show

transformAge :: Either (String, Error) (String, String) -> Either (String, Error) (String, String)
transformAge a = case a of
  Right (n, m) -> if ((all isDigit m) && (length m) > 0) then Right (n, m)
                                     else Left (n, IncorrectDataError m)
  Left c -> Left c

parsePerson :: String -> Either Error Person
parsePerson rawData = 
  let 
  attrs = splitOn "\n" rawData
  attrs' = map (\x -> case (splitOn " = " x) of 
                     m : n : [] -> Right (m, n)
                     m : [] -> Left (m, ParsingError)
              )
              attrs

  firstName' = filter (\x -> case x of 
                                           Right (n, m) -> (n == "firstName")
                                           Left  (n, m) -> (n == "firstName")
                                           )
                                           attrs'
  lastName' = filter (\x -> case x of 
                                           Right (n, m) -> (n == "lastName")
                                           Left  (n, m) -> (n == "lastName")
                                           )
                                           attrs'
  age' = filter (\x -> case x of 
                                           Right (n, m) -> (n == "age")
                                           Left  (n, m) -> (n == "age")
                                           )
                                           attrs'

  firstName = case firstName' of
                   [] -> Left ("firstName", IncompleteDataError)
                   x : _ -> x

  lastName = case lastName' of
                   [] -> Left ("lastName", IncompleteDataError)
                   x : _ -> x
  age = case age' of
                   [] -> Left ("age", IncompleteDataError)
                   x : _ -> x
  --lastName = head (map snd (filter (\x -> case x of (n, m) -> (n == "lastName") attrs')))
  --age = head (map snd (filter (\x -> case x of (n, m) -> (n == "age") attrs')))
  in
  case (firstName, lastName, (transformAge age)) of
       (Right (_, m), Right (_, m'), Right (_, m'')) -> Right $ Person { firstName=m, lastName=m', age=(read m'' :: Int) }
       (Left (_, m), _, _) -> Left (m)
       (_, Left (_, m), _) -> Left (m)
       (_, _, Left (_, m)) -> Left (m)
