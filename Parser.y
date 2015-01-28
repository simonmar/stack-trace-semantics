{
module Parser (parse) where
import Syntax
import Lexer
}

%tokentype { Token }
%token
 'let' { Let }
 'in'  { In }
 'push' { Push }
 '+'   { Sym '+' }
 '='   { Sym '=' }
 ';'   { Sym ';' }
 '('   { Sym '(' }
 ')'   { Sym ')' }
 '\\'  { Sym '\\' }
 '.'   { Sym '.' }
 VAR   { Var $$ }
 INT   { Int $$ }

%name parse

%%

prog	:: { [Bind] }
 	: {- empty -}		{ [] }
	| bind ';' prog		{ $1 : $3 }

bind 	:: { Bind }
	: VAR '=' expr		{ ($1, $3) }

expr	:: { Expr }
	: expr2 '+' expr	{ EPlus $1 $3 }
	| expr1			{ $1 }

expr1	:: { Expr }
	: 'let' bind 'in' expr	{ ELet $2 $4 }
        | 'push' VAR expr       { EPush $2 $3 }
	| '\\' VAR '.' expr	{ ELam $2 $4 }
	| expr2			{ $1 }

expr2	:: { Expr }
	: expr2 VAR		{ EApp $1 $2 }
	| expr3			{ $1 }

expr3	:: { Expr }
	: VAR			{ EVar $1 }
	| INT			{ EInt $1 }
	| '(' expr ')'		{ $2 }

{
happyError = error "parse error"
}  
