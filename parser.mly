/* Analyse Syntaxique
 */

%{
	#open "arbre"
%}

%token <string> NUM
%token PLUS MOINS FOIS DIV
%token LPAR RPAR
%start Main
%type <string> Main
%%

Main :
	LPAR Expr RPAR  { $2 }
;

Expr :
	| APP Expr Expr	{}
	| Cst			{}
;;
  
