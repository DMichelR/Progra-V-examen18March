module Main (main) where

import qualified Data.Map as Map
import DiegoMichelTest
import ParserBool

main :: IO ()
main = do
  putStrLn "Ingrese una expresión:"
  input <- getLine
  let expr = parseExpr input
  print expr
  let vars = Map.fromList [('A', True), ('B', False), ('C', True)]
  let result = evalExpr expr vars
  print result
