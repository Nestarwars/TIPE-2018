(**************************************************************************)
(*                       TIPE 2018 - Compilation                          *)
(*               Kineider    Bourret-Mathieu    Laborier                  *)
(*                                                                        *)
(*                          ANALYSE LEXICALE                              *)
(*                                                                        *)
(*                  This file is under WTFPL License                      *)
(**************************************************************************)


(* type des lexèmes reconnus, ici on se limite à l'arithmétique basique *)
type lexeme =
  | PLUS
  | MOINS
  | FOIS
  | DIV
  | eof
  | NUM of int
  | VAR of string
;;

type lexbuf = lexeme list ;;

(* analyse_lex exécute l'analyse lexicale de la chaine en entrée, et la rend dans un lexbuf *)
value analyse_lex : string -> lexbuf ;;
