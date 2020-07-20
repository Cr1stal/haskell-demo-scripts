import Data.List
import Data.Char

data Error = ParsingError | IncompleteDataError | IncorrectDataError String

data Person = Person { firstName :: String, lastName :: String, age :: Int }

parsePerson :: String -> Either Error Person
parsePerson s = xg
  where
    xa = wordsWhen (== '\n') s
    xb = map (wordsWhen (== '=')) xa
    xc = filter (\x -> (length x) == 2) xb
    xd = map (\(x : y : xs) -> (trim x) : (trim y) : xs) xc
    xe = map (,) xd
    xf = filter (\(x,_) -> x == "firstName" || x == "lastName" || x == "age") xe
    xg = foldr (\(k,v) acc -> case k of 
                                "firstName" -> acc { firstName = v }
                                "lastName" -> acc { lastName = v }
                                "age" -> acc { age = (read v :: Int) }
				)
				(Person {}) xf

trim = dropWhileEnd isSpace . dropWhile isSpace

wordsWhen     :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : wordsWhen p s''
                            where (w, s'') = break p s'

