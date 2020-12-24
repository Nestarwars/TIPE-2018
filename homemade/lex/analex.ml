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
  | MOINS
  | FOIS
  | DIV
  | eof
  | NUM of int
;;

(* the list of recognized lexems *)
type lexbuf == lexeme list
;;
    
(* analyse_lex_buf takes a buffer into a list of lexems, type lexbuf,
it read each element seperatly and flag each on with its type *)
let analyse_lex_bis buffer =
  let rec acc buf (lexbuf : lexbuf) i =
    match nth_char buf i with
    | `.` -> lexbuf
    | ` ` -> acc buf lexbuf (i+1)
    | `+` -> PLUS::(acc buf lexbuf (i+1))
    | `-` -> MOINS::(acc buf lexbuf (i+1))
    | `*` -> FOIS::(acc buf lexbuf (i+1))
    | `/` -> DIV::(acc buf lexbuf (i+1))
    | x -> (NUM (int_of_char x - 48))::(acc buf lexbuf (i+1))
  in acc buffer [] 0
;;
  
(* conc_c_str concatenates a character with a string *)
let cons_c_str char str = let c = string_of_char char in
			  c^str
;;

(* space_detect takes a string and return a list of the indexes where the spaces are *)
let space_detect str =
  let rec space_aux phrase i l =
    if i < (string_length phrase)
    then
      if (nth_char phrase i) = ` `
      then i :: (space_aux phrase (i+1) l)
      else space_aux phrase (i+1) l
    else l
  in
  space_aux str 0 []
;;

(* split_str takes a string, detects the spaces and splits on the spaces *)
let split str =
  let rec split_aux phrase l index =
    match index with
    | [] -> l
    | [x] -> (sub_string phrase 0 (x-1))::(sub_string phrase x ((string_length phrase) - 1))::l
    | x::y::r -> (sub_string phrase x (y-1))::(split_aux phrase l r)
  in
  split_aux str [] (space_detect str)
;;
  
