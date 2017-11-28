
(* Home made compiler
 *)

#open "lexer";;  (* string -> lexbub *)
#open ???;; (* lexbuf -> lextree *)
#open "make_code";; (* lextree -> string *)

(* compiler: string -> string
 *)
let compiler src =
	make_code ??? lexing src
;;
