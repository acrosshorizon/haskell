{-# LANGUAGE NoImplicitPrelude #-}
module Parser where

import           Base
import           Functor
import           Applicative
import           Exercises
import           Traversable
import           Folds
import           ExercisesW8
import           ParserW8

import           Prelude                        ( reads )

-- | Parse a tuple with three integers
--
-- /Hint/: Same pattern as parseIntTuple2, now just with 3 things to parse ..
--
-- >>> parse parseIntTuple3 "(10,2,3)"
-- Just ("",(10,2,3))
--
-- >>> parse parseIntTuple3 "(1,2)"
-- Nothing
parseIntTuple3 :: Parser (Int, Int, Int)
parseIntTuple3 = liftA3 (,,) (open '(') item (item <* is ')')

-- | Repeat a parse some number of times
--
-- /Hint:/ Have a look at the types. I think we have seen something similar in ExercisesW8.hs..
--
-- /Hint 2/: We are /replicating/ the parser .. but wait it has an Applicative effect ..
--
-- >>> parse (thisMany 3 $ is 'a') "aaabc"
-- Just ("bc","aaa")
--
-- >>> parse (thisMany 4 $ is 'a') "aaabc"
-- Nothing
thisMany :: Int -> Parser a -> Parser [a]
thisMany = replicateA
-- thisMany n p = sequence (replicate n p)

-- | Parse a fixed length array of integers
--
-- /Hint/: thisMany runs a parser a fixed number of times
--
-- /Hint 2/: liftA2 can be used to apply a function across all the parse results
--
-- >>> parse (fixedArray 3) "[1,2,3]"
-- Just ("",[1,2,3])
--
-- >>> parse (fixedArray 4) "[1,2,3,10]"
-- Just ("",[1,2,3,10])
--
-- >>> parse (fixedArray 2) "[1,2,3]"
-- Nothing
fixedArray :: Int -> Parser [Int]
fixedArray 0 = [] <$ (is '[' *> is ']')
fixedArray i = liftA2 (:) (open '[') (thisMany (i - 1) item <* is ']')

-- | Write a function that parses the given string (fails otherwise).
--
-- /Hint/: Use traverse...
--
-- >>> parse (string "hello") "hello bob"
-- Just (" bob","hello")
-- >>> parse (string "hey") "hello bob"
-- Nothing
string :: String -> Parser String
string = traverse is

-- | Return a parser that tries the first parser:
--
--   * if the first parser succeeds then use the first parser parser; or
--
--   * if the first parser fails, try the second parser.
--
-- For this specific question, deconstruct the Parser, but it shouldn't be
-- necessary for the other questions.
--
-- >>> parse (is 'a' ||| is 'c') "abc"
-- Just ("bc",'a')
--
-- >>> parse (is 'a' ||| is 'c') "cba"
-- Just ("ba",'c')
--
-- >>> parse (is 'a' ||| is 'c') "123"
-- Nothing
(|||) :: Parser a -> Parser a -> Parser a
p1 ||| p2 = Parser $ \x -> case parse p1 x of
    Nothing -> parse p2 x
    _       -> parse p1 x

-- | Parse a Nothing value. Parse the string "Nothing", if succeeds return Nothing
--
-- /Hint/: Use string and $>
--
-- >>> parse nothing "something"
-- Nothing
-- >>> parse nothing "Nothing"
-- Just ("",Nothing)
nothing :: Parser (Maybe a)
nothing = string "Nothing" $> Nothing

-- | Parse a Just value. Parse the string "Just ", followed by the given parser
--
-- >>> parse (just int) "Just 1"
-- Just ("",Just 1)
-- >>> parse (just int) "Nothing"
-- Nothing
just :: Parser a -> Parser (Maybe a)
just p = string "Just " *> (Just <$> p)
-- just p = string "Just " *> (pure <$> p)

-- | Parse a 'Maybe'
-- | This is named maybeParser due to a nameclash with Prelude.maybe
--
-- /Hint/: What does (|||) do?
--
-- /Hint 2/: Maybe types can only be just or nothing
--
-- >>> parse (maybeParser int) "Just 1"
-- Just ("",Just 1)
--
-- >>> parse (maybeParser int) "Nothing"
-- Just ("",Nothing)
--
-- >>> parse (maybeParser int) "Something Else"
-- Nothing
maybeParser :: Parser a -> Parser (Maybe a)
maybeParser p = nothing ||| just p

-- | Parse an array of length less then or equal to n
--
-- /Hint/: Use foldr, fixedArray and |||
--
-- /Hint 2/: atMostArray 2 = fixedArray 0 ||| fixedArray 1 ||| fixedArray 2
--
-- >>> parse (atMostArray 3) "[1,2,3,4,5]"
-- Nothing
-- >>> parse (atMostArray 10) "[1,2,3,4,5]"
-- Just ("",[1,2,3,4,5])
atMostArray :: Int -> Parser [Int]
atMostArray n = foldr (|||) (fixedArray 0) (fixedArray <$> [1 .. n])

-- | Parse a sequence of arrays of length less then or equal to 3
--
-- /Hint/: Use traverse, parse and atMostArray
--
-- >>> validArrays ["[1,2]", "[1,2,3]", "[1]", "[]"]
-- Just [("",[1,2]),("",[1,2,3]),("",[1]),("",[])]
--
-- >>> validArrays ["[1,2]", "[1,2,3]", "[1]", "[]", "[1,2,3,4]"]
-- Nothing
validArrays :: [String] -> Maybe [(String, [Int])]
validArrays = traverse (parse $ atMostArray 3)
-- validArrays = sequence . (parse atMostArray <$>)

-- | Sum the values of a sequence of arrays of length less then or equal to 3
--
-- /Hint/: We want to /map/ a function (String, [Int]) -> Int into 2 layers of /nested/ Functors
-- validArrays :: [String] -> Maybe [(String, [Int])]
-- parseAndSum :: [String] -> Maybe [Int]
--
-- >>> parseAndSum ["[1,2]", "[1,2,3]", "[1]", "[]"]
-- Just [3,6,1,0]
--
-- >>> parseAndSum ["[1,2]", "[1,2,3]", "[1]", "[]", "[1,2,3,4]"]
-- Nothing
parseAndSum :: [String] -> Maybe [Int]
parseAndSum = nestedMap (sum . snd) . validArrays
-- parseAndSum s = nestedMap (sum . snd) (validArrays s)
