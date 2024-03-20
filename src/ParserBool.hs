module ParserBool (Expr (..), parseExpr) where

data Expr
  = Var Char
  | Not Expr
  | And Expr Expr
  | Or Expr Expr
  deriving (Show)

parseExpr :: String -> Expr
parseExpr = fst . parseOr . filter (/= ' ')
  where
    parseOr :: String -> (Expr, String)
    parseOr str =
      let (expr1, rest1) = parseAnd str
       in case rest1 of
            ('∨' : rest2) ->
              let (expr2, rest3) = parseOr rest2
               in (Or expr1 expr2, rest3)
            _ -> (expr1, rest1)

    parseAnd :: String -> (Expr, String)
    parseAnd str =
      let (expr1, rest1) = parseFactor str
       in case rest1 of
            ('∧' : rest2) ->
              let (expr2, rest3) = parseAnd rest2
               in (And expr1 expr2, rest3)
            _ -> (expr1, rest1)

    parseFactor :: String -> (Expr, String)
    parseFactor ('¬' : rest) =
      let (expr, rest') = parseFactor rest
       in (Not expr, rest')
    parseFactor ('(' : rest) =
      let (expr, rest') = parseOr rest
          (_ : rest'') = rest' -- skip the closing parenthesis
       in (expr, rest'')
    parseFactor str = parseVar str

    parseNot :: String -> (Expr, String)
    parseNot ('¬' : rest) =
      let (expr, rest') = parseNot rest
       in (Not expr, rest')
    parseNot ('(' : rest) = parseOr rest
    parseNot str = parseVar str

    parseVar :: String -> (Expr, String)
    parseVar (c : rest)
      | c `elem` ['A', 'B', 'C'] = (Var c, rest)
      | otherwise = error ("Carácter no válido: " ++ [c])
    parseVar [] = error "Expresión vacía"