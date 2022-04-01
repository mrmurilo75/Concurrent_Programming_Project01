module Main (main) where

import Parser
import System.Environment

main :: IO ()
main =
  do
    args <- getArgs
    mapM (putStrLn . map (
             \ch ->
               if ch == '(' then '{' else if ch == ')' then '}' else ch
             ) . show) $ map parse_poly args
    return ()
