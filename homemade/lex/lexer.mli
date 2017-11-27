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

(* lexing executes the lexical analysis and returns a lexbuf *)
value lexing : string -> lexbuf ;;
