{
module Lexer (Token(..), alexScanTokens) where
}

%wrapper "basic"

$digit = 0-9			-- digits
$alpha = [a-zA-Z]		-- alphabetic characters

tokens :-
  $white+ 				;
  "--".*				;
  let					{ \s -> Let }
  in					{ \s -> In }
  push                                  { \s -> Push }
  $digit+				{ \s -> Int (read s) }
  [\;\.\\\=\+\-\*\/\(\)]		{ \s -> Sym (head s) }
  $alpha [$alpha $digit \_ \']*		{ \s -> Var s }

{
-- Each right-hand side has type :: String -> Token

-- The token type:
data Token
 = Let
 | In
 | Push
 | Sym Char
 | Var String
 | Int Int
 deriving (Eq,Show)

}
