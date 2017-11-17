

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


let alphabet_machine s alphabet blanks =

  let trans_letter = make_transList_oneNode alphabet "state_letter"
  and trans_blank  = make_transList_oneNode blanks   "state_blank"
  and trans_list = merge_transLists trans_letter trans_blank in

  let state_init = make_state "epsilon" trans_list
  and state_letter = make_state "state_letter" trans_list
  and state_blank  = make_state "state_blank"  trans_list in

  let state_list = [state_letter;state_blank] in

  let alpha_machine = make_machine state_list state_init state_list in
  alpha_machine
;;
