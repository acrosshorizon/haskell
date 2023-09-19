module Main (main) where

import Parser (prettifyBinaryTree)

main :: IO ()
main = getLine >>= putStrLn . prettifyBinaryTree >> main
