/* Analyse Syntaxique
   Abstr(Lexemes) -> L2
*/
%{ type arbre = 
     |Vide
     |N of arbre * string * arbre
   ;;
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
	NUM		      	{N(Vide,$1,Vide)}
  | LPAR Expr RPAR		{ $2 }
  | PLUS Expr Expr		{ N($2,"PLUS",$3)}
  | MOINS Expr Expr		{ N($2,"MOINS",$3)}
  | FOIS Expr Expr		{ N($2,"FOIS",$3)}
  | DIV Expr Expr		{ N($2,"DIV",$3)}
;;
  
