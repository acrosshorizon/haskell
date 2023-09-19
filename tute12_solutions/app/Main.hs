import           Instances
import           JSON
import           Parser
import           Prelude
import           System.Environment

import           Control.Monad.State
import           Data.List

import           Debug.Trace

-- | Prompt user for a JSON file, parse it and pretty print the result.
--
-- /Tip:/ use @getArgs@, @readJsonFile@ and @putStrLn@
--
-- You can run this function in GHCi by calling:
-- > :main "input.json"
main :: IO ()
main = do
  (f : _) <- getArgs
  let x = trace ("f=" ++ show f) False
  r <- readJsonValue f
  case r of
    Result _ json -> putStr $ tail $ pretty json 0 ++ "\n"
    Error e       -> print $ "ERROR: " ++ show e


pretty :: JsonValue -> Int -> String
pretty JsonNull         i = "null"
pretty JsonTrue         i = "true"
pretty JsonFalse        i = "false"
pretty (JsonString   s) i = "\"" ++ s ++ "\""
pretty (JsonArray    a) i = wrap "[" "]" pretty a i
pretty (JsonObject   o) i = wrap "{" "}" prettyPair o i
pretty (JsonRational v) i = indent i ++ show v

prettyPair :: (String, JsonValue) -> Int -> String
prettyPair (key, val) i = indent i ++ key ++ ": " ++ pretty val (succ i)

wrap :: String -> String -> (a -> Int -> String) -> [a] -> Int -> String
wrap start end p o i = indent i ++ start ++ prettyList p o i ++ indent i ++ end

prettyList :: (a -> Int -> String) -> [a] -> Int -> String
prettyList p o i = intercalate "," $ map (flip p (succ i)) o

indent :: Int -> String
indent i = "\n" ++ replicate (i * 2) ' '
