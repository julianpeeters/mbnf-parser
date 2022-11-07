module Main (main) where

import Mbnf.Parser (parseMbnf)
import System.Environment (getArgs)

main :: IO ()
main = do
    paths <- getArgs
    schema <- traverse parseMbnf paths
    putStrLn $ show schema
