import Text.Read (readMaybe)
import Data.Char (isSpace)
import Data.List (dropWhileEnd)

data Error = ParsingError | IncompleteDataError | IncorrectDataError String deriving Show

data Person = Person { firstName :: String, lastName :: String, age :: Int } deriving Show

trim :: String -> String
trim = dropWhileEnd isSpace . dropWhile isSpace

parseField :: String -> Either Error (String, String)
parseField s = case span (/= '=') s of
  (f, '=':v) -> if not (null v) 
                then Right (trim f, trim v)
                else Left ParsingError
  _ -> Left ParsingError

lookupVal :: String -> [(String, String)] -> Either Error String
lookupVal name fields = case lookup name fields of
                        Just val -> Right val
                        Nothing  -> Left IncompleteDataError

readVal :: String -> Either Error Int
readVal a = case readMaybe a of
            Just s -> Right s
            Nothing -> Left $ IncorrectDataError a

parsePerson :: String -> Either Error Person
parsePerson s = do
    fields <- mapM parseField (lines s)
    fn <- lookupVal "firstName" fields
    ln <- lookupVal "lastName" fields
    age <- lookupVal "age" fields >>= readVal
    return $ Person fn ln age
