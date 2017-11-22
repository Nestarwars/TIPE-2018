(**************************************************************************)
(*                        TIPE 2018 - Compiling                           *)
(*               Kineider    Bourret-Mathieu    Laborier                  *)
(*                                                                        *)
(*                          LEXICAL ANALYSIS                              *)
(*                                                                        *)
(*                  This file is under WTFPL License                      *)
(**************************************************************************)


(* the recognized lexems *)
type lexeme =
  | PLUS
  | MINUS
  | TIMES
  | DIV
  | EOF
  | NUM of int
  | VAR of string
;;

type lexbuf = lexeme list ;;

(* analyse_lex exécute l'analyse lexicale de la chaine en entrée, et la rend dans un lexbuf *)
value analyse_lex : string -> lexbuf ;;
