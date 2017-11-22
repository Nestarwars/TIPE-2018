type lexeme =
  | PLUS
  | MOINS
  | FOIS
  | DIV
  | eof
  | NUM of int
;;

type lexbuf == lexeme list
;;
    


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
  

let cons_c_str char str = let c = string_of_char char in
			  c^str
;;
  
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

let split str =
  let rec split_aux phrase l index =
    match index with
    | [] -> l
    | [x] -> (sub_string phrase 0 (x-1))::(sub_string phrase x ((string_length phrase) - 1))::l
    | x::y::r -> (sub_string phrase x (y-1))::(split_aux phrase l r)
  in
  split_aux str [] (space_detect str)
;;
  
