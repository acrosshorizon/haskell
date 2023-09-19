-- | Implement a JSON parser.
module JSON where

import           Data.Char

import           Control.Applicative
import           Instances
import           Parser

-- | Associative container type.
type Assoc = [(String, JsonValue)]

-- | JSON value representation.
data JsonValue =
     JsonString String
   | JsonRational !Rational
   | JsonObject Assoc
   | JsonArray [JsonValue]
   | JsonTrue
   | JsonFalse
   | JsonNull
  deriving (Show, Eq)

-- | Write a function that applies the first parser, runs the third parser
-- keeping the result, then runs the second parser and produces the obtained
-- result.
--
-- /Hint/: Use the monad instance.
--
-- >>> parse (between (is '[') (is ']') char) "[a]"
-- Result >< 'a'
--
-- >>> isErrorResult (parse (between (is '[') (is ']') char) "[abc]")
-- True
--
-- >>> isErrorResult (parse (between (is '[') (is ']') char) "[abc")
-- True
--
-- >>> isErrorResult (parse (between (is '[') (is ']') char) "abc]")
-- True
between :: Parser o -> Parser c -> Parser a -> Parser a
between o c a = do
  o
  r <- a
  c
  pure r

-- | Write a function that applies the given parser in between the two given
-- characters.
--
-- /Hint/: Use 'between' and 'charTok'.
--
-- >>> parse (betweenCharTok '[' ']' char) "[a]"
-- Result >< 'a'
-- >>> parse (betweenCharTok '[' ']' (charTok 'a')) "[ a ] "
-- Result >< 'a'
--
-- >>> isErrorResult (parse (betweenCharTok '[' ']' char) "[abc]")
-- True
--
-- >>> isErrorResult (parse (betweenCharTok '[' ']' char) "[abc")
-- True
--
-- >>> isErrorResult (parse (betweenCharTok '[' ']' char) "abc]")
-- True
betweenCharTok :: Char -> Char -> Parser a -> Parser a
betweenCharTok o c = between (charTok o) (charTok c)

-- | Parse a JSON string. Handle double-quotes, special characters, hexadecimal
-- characters.
--
-- /Hint/: Use 'between', 'is', 'charTok', 'noneof', 'jsonSpecial'.
--
-- /Hint/: The inner parser needs to /fail/ in order to trigger the second
-- delimiter.
--
-- >>> parse jsonString "\" abc\""
-- Result >< " abc"
--
-- >>> parse jsonString "\"abc\"def"
-- Result >def< "abc"
--
-- >>> parse jsonString "\"abc\"   def"
-- Result >def< "abc"
--
-- >>> isErrorResult (parse jsonString "abc")
-- True
--
-- >>> isErrorResult (parse jsonString "\"\\abc\"def")
-- True
jsonString :: Parser String
jsonString = between (is '"') (charTok '"') (list $ noneof "\"\\")

-- | Parse a JSON rational.
--
-- /Hint/: Use 'readFloats', 'tok'.
--
-- >>> parse jsonNumber "234"
-- Result >< 234 % 1
--
-- >>> parse jsonNumber "-234"
-- Result >< (-234) % 1
--
-- >>> parse jsonNumber "123.45"
-- Result >< 2469 % 20
--
-- >>> parse jsonNumber "-123"
-- Result >< (-123) % 1
--
-- >>> parse jsonNumber "-123.45"
-- Result >< (-2469) % 20
--
-- >>> isErrorResult (parse jsonNumber "-")
-- True
--
-- >>> isErrorResult (parse jsonNumber "abc")
-- True
jsonNumber :: Parser Rational
jsonNumber = tok $ P
  (\s -> case readFloats s of
    Just (f, r) -> Result r f
    Nothing     -> Error $ UnexpectedString ("Not a Number" ++ s)
  )

-- | Parse a JSON true literal.
--
-- /Hint/: Use 'stringTok'.
--
-- >>> parse jsonTrue "true"
-- Result >< "true"
--
-- >>> isErrorResult (parse jsonTrue "TRUE")
-- True
jsonTrue :: Parser String
jsonTrue = stringTok "true"

-- | Parse a JSON false literal.
--
-- /Hint/: Use 'stringTok'.
--
-- >>> parse jsonFalse "false"
-- Result >< "false"
--
-- >>> isErrorResult (parse jsonFalse "FALSE")
-- True
jsonFalse :: Parser String
jsonFalse = stringTok "false"

-- | Parse a JSON null literal.
--
-- /Hint/: Use 'stringTok'.
--
-- >>> parse jsonNull "null"
-- Result >< "null"
--
-- >>> isErrorResult (parse jsonNull "NULL")
-- True
jsonNull :: Parser String
jsonNull = stringTok "null"

-- | Write a parser that parses between the two given characters, separated by
-- a comma char ','.
--
-- /Hint/: Use 'betweenCharTok', 'sepby' and 'commaTok'.
--
-- >>> parse (betweenSepbyComma '[' ']' lower) "[a]"
-- Result >< "a"
--
-- >>> parse (betweenSepbyComma '[' ']' lower) "[]"
-- Result >< ""
--
-- >>> parse (betweenSepbyComma '[' ']' lower) "[a,b,c]"
-- Result >< "abc"
--
-- >>> isErrorResult (parse (betweenSepbyComma '[' ']' lower) "[A]")
-- True
--
-- >>> isErrorResult (parse (betweenSepbyComma '[' ']' lower) "[abc]")
-- True
--
-- >>> isErrorResult (parse (betweenSepbyComma '[' ']' lower) "[a")
-- True
--
-- >>> isErrorResult (parse (betweenSepbyComma '[' ']' lower) "a]")
-- True
betweenSepbyComma :: Char -> Char -> Parser a -> Parser [a]
betweenSepbyComma o c p = betweenCharTok o c (sepby p commaTok)

-- | Parse a JSON array.
--
-- /Hint/: Use 'betweenSepbyComma' and 'jsonValue'.
--
-- >>> parse jsonArray "[]"
-- Result >< []
--
-- >>> parse jsonArray "[true]"
-- Result >< [JsonTrue]
--
-- >>> parse jsonArray "[true, \"abc\"]"
-- Result >< [JsonTrue,JsonString "abc"]
--
-- >>> parse jsonArray "[true, \"abc\", []]"
-- Result >< [JsonTrue,JsonString "abc",JsonArray []]
--
-- >>> parse jsonArray "[true, \"abc\", [false]]"
-- Result >< [JsonTrue,JsonString "abc",JsonArray [JsonFalse]]
jsonArray :: Parser [JsonValue]
jsonArray = betweenSepbyComma '[' ']' jsonValue

-- | Parse a JSON object.
--
-- /Hint/: Use 'jsonString', 'charTok', 'betweenSepbyComma' and 'jsonValue'.
--
-- /Hint/: Remember the type of 'Assoc' = @[(String, JsonValue)]@.
--
-- /Hint/: Use anonymous apply '<*' to omit tokens.
--
-- >>> parse jsonObject "{}"
-- Result >< []
--
-- >>> parse jsonObject "{ \"key1\" : true }"
-- Result >< [("key1",JsonTrue)]
--
-- >>> parse jsonObject "{ \"key1\" : true , \"key2\" : false }"
-- Result >< [("key1",JsonTrue),("key2",JsonFalse)]
--
-- >>> parse jsonObject "{ \"key1\" : true , \"key2\" : false } xyz"
-- Result >xyz< [("key1",JsonTrue),("key2",JsonFalse)]
jsonObject :: Parser Assoc
jsonObject = betweenSepbyComma '{' '}' fields
  where fields = liftA2 (,) (jsonString <* charTok ':') jsonValue

-- | Parse a JSON value.
--
-- /Hint/: Use 'spaces', 'jsonNull', 'jsonTrue', 'jsonFalse', 'jsonArray',
-- 'jsonString', 'jsonObject' and 'jsonNumber'.
--
-- /Hint/: Use anonymous map '<$' to "type-hint" your literals.
--
-- /Hint/: Use fmap '<$>' to "type-hint" your values.
--
-- >>> parse jsonValue "true"
-- Result >< JsonTrue
--
-- >>> parse jsonObject "{ \"key1\" : true , \"key2\" : [7, false] }"
-- Result >< [("key1",JsonTrue),("key2",JsonArray [JsonRational (7 % 1),JsonFalse])]
--
-- >>> parse jsonObject "{ \"key1\" : true , \"key2\" : [7, false] , \"key3\" : { \"key4\" : null } }"
-- Result >< [("key1",JsonTrue),("key2",JsonArray [JsonRational (7 % 1),JsonFalse]),("key3",JsonObject [("key4",JsonNull)])]
jsonValue :: Parser JsonValue
jsonValue =
  (JsonNull <$ jsonNull)
    ||| (JsonTrue <$ jsonTrue)
    ||| (JsonFalse <$ jsonFalse)
    ||| (JsonArray <$> jsonArray)
    ||| (JsonString <$> jsonString)
    ||| (JsonObject <$> jsonObject)
    ||| (JsonRational <$> jsonNumber)

-- | Read a file into a JSON value.
--
-- /Hint/: Use 'readFile' and 'jsonValue'.
readJsonValue :: FilePath -> IO (ParseResult JsonValue)
readJsonValue f = parse jsonValue <$> readFile f
