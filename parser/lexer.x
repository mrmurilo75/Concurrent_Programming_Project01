{
module Lexer
  ( Token (..)
  , lex_poly
  ) where
}

%wrapper "basic"

tokens :-
  $white+ ;

  \+ { \_ -> Plus }
  \* { \_ -> Times }
  \^ { \_ -> Power }

  [a-zA-Z]+ { \var -> Var var }
  [0-9]+    { \int_literal -> (IntLiteral . read) int_literal }

{
data Token
  = Plus
  | Times
  | Power
  | IntLiteral Int
  | Var String
  deriving (Show)

lex_poly :: String -> [Token]
lex_poly = alexScanTokens
}
