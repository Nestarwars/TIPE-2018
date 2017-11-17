
#open "regexpr";;
#open "stateMachine";;


(* construct_from_regexpr: regexpr -> stateMachine
   Construct a state machine which recognize the language described by the given regexpr.
 *)
let construct_from_regexpr ex = ()
;;

(* construct: string -> stateMachine
   Construct a state machine which recognize the language described by the given string.
 *)
let construct s =
  construct_from_regexpr (make_regexpr s)
;;



