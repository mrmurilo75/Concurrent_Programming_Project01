{
module Parser (parse_poly) where

import Lexer
}

%name parser
%tokentype { Token }
%error { parse_error }

%token
  '+' { Plus }
  '*' { Times }
  '^' { Power }

  var         { Var $$ }
  int_literal { IntLiteral $$ }

%%

Expr
  : Factor          { [$1] }
  | Factor '+' Expr { $1:$3 }

Factor
  : int_literal             { ($1, []) }
  | int_literal '*' VarProd { ($1, $3) }
  | VarProd                 { (1, $1) }

VarProd
  : Var             { [$1] }
  | Var '*' VarProd { $1:$3 }

Var
  : var                 { ($1, 1) }
  | var '^' int_literal { ($1, $3) }

{
parse_error :: [Token] -> a
parse_error (x:xs) = error ("error: unexpected token: " ++ show x)
parse_error [] = error "error: unexpected end of line"

parse_poly = parser . lex_poly
}
