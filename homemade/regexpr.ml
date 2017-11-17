

type regexpr =
| zero
| one
| letter of char
| plus of regexpr*regexpr
| concat of regexpr*regexpr
| star of regexpr
;;


(* print_regexpr: regexpr -> unit
   prints a regexpr to the screen.
 *)
let rec print_regexpr e =
  match
    e
  with
  | zero -> print_char `0`
  | one -> print_char `1`
  | letter( lett ) -> print_char lett
  | plus( l,r ) ->
    print_string "(";
    print_regexpr l;
    print_string " + ";
    print_regexpr r;
    print_string ")"

  | concat( l,r ) ->
    print_string "(";
    print_regexpr l;
    print_string ".";
    print_regexpr r;
    print_string ")"

  | star( ex ) ->
    print_string "(";
    print_regexpr ex
;;


(* make_regexpr: string -> regexpr
 *)
let rec make_regexpr s =
  ()
;;

