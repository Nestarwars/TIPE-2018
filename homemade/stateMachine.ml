
type atom == char;; (* text atom == letter *)

type stateName == string;;
type trans == atom*stateName;; (* Transition: "this atom leads to this state". *)

type state == stateName*(trans list);; (* Basically a graph node. *)

type stateMachine = {
  Q: state list; (* Machine state list *)
  q: state;      (* Machine initial state id *)
  F: state list; (* Machine end states ids list *)
  mutable act: state; (* Actual machine state id *)
};;


let emptyStateList () = ([]:state list);;
let emptyState () = ( (("":stateName), ([]:trans list)) : state);;

let make_machine_empty () =
  {
    Q = emptyStateList ();
    q = emptyState ();
    F = emptyStateList ();
    act = emptyState ();
  }
;;

(* make_machine: state list -> state -> state list -> stateMachine
   Creates a stateMachine with specified parameters.
 *)
let make_machine (stateList:state list) (initialState:state) (endStates:state list) =
  {
    Q=stateList;
    q=initialState;
    F=endStates;
    act=initialState;
  }
;;

(* machine_init: stateMachine -> unit
   Reset the actual state of the stateMachine.
 *)
let machine_init m =
  m.act <- m.q
;;




(* make_trans: atom -> stateName -> trans
   Short for making a transition from atom & state name.
 *)
let make_trans at sname =
  ( (at,sname) : trans )
;;

(* make_transList: atom list -> stateName list -> trans list
   make_transList [t0 ... tn] [n0 ... nn] == [make_trans t0 n0 ... make_trans tn nn].
 *)
let make_transList (ats: atom list) (snames: stateName list) =
  map2 make_trans ats snames
;;

(* make_transList_oneNode: atom list -> stateName
   Transition from several atoms to unique state.
 *)
let rec make_transList_oneNode (ats: atom list) (sname: stateName) =
  match
    ats
  with
  | [] -> []
  | at::r -> (make_trans at sname) :: make_transList_oneNode r sname
;;

(* merge_transLists: trans list -> trans list -> trans list
 *)
let rec merge_transLists (l1: trans list) (l2: trans list) =
  union l1 l2
;;



(* make_state: stateName -> trans list -> state
   Makes a state (node) with the given name & the given transition list.
 *)
let make_state sname trlist =
  ( (sname,trlist): state )
;;

let getStateName (state:state) =
  fst state
;;
let getTransList (state:state) =
  snd state
;;


(* isBlankChar: char -> bool
   isBlankChar c return true if c is blank character ie ` `, `\n` or `\t`.
 *)
let isBlankChar ch =
  ch=` ` || ch=`\n` || ch=`\t`
;;



(* step: stateMachine -> atom -> bool
   step m c process a step in the state machine.
   Return true if machine stops, false otherwise.
 *)
let step (m:stateMachine) (at:atom) =
  let trans = getTransList m.act in
  try
    let newName = assoc at trans in
    let newTrans = assoc newName m.Q in
    m.act <- newName,newTrans;
    false
  with
  | Not_found -> true (* Oops!: we're on a state with no exit for this token. This can be an error or not(end state) *)
;;

(* Note:
   je sais pas trop du coup je pars là du principe que si l'exec s'arrête (cause:caractère illégal) sur un noeud final, c'est bon.
 *)
(* run_machine: stateMachine -> string -> bool*string
   Execute the machine consecutively on each character of the string.
   Return true if execution was successful, false otherwise, coupled with the parsed word in every case.
 *)
let rec run_machine (m:stateMachine) s =
  let l = string_length s in
  let parsed_expr = ref "" in

  let rec parse_aux i =
    if( i < l )
    then(
      let at = ((nth_char s i): atom) in (* get actual analyzed char *)
      let err = step m at in (* process a step forward *)
      
      parsed_expr := !parsed_expr ^ (string_of_char at);
      if( not err )
      then(
	(* Machine continues. *)
	parse_aux (i+1)
      )
      else(
	(* Machine stopped. Are we on an end point? *)
	if( mem m.act m.F )
	then( false,!parsed_expr )
	else( false,!parsed_expr )
      )
    )
    else(
      true,!parsed_expr (* true for empty. *)
    )
  in

  parse_aux 0
;;





let alphabet_machine alphabet =

  let trans_letter = make_transList_oneNode alphabet "state_letter" in

  let state_init = make_state "epsilon" trans_letter in
  let state_letter = make_state "state_letter" trans_letter in

  let state_list = [state_letter] in

  let alpha_machine = make_machine state_list state_init state_list in
  alpha_machine
;;




(* Tests *)

(* Machine states *)
let state_init = make_state "epsilon" (make_transList [`0`;`1`] ["q0";"q1"]);;
let state_0    = make_state "q0"      (make_transList [`0`;`1`] ["q0";"q1"]);;
let state_1    = make_state "q1"      (make_transList [`0`;`1`] ["q0";"q1"]);;
let state_list = [ state_init; state_0; state_1 ];;
let state_end  = [ state_0; state_1 ];; (* not used for now. *)

(* recognise binary string: (0?1?)* *)
let bin_machine = make_machine state_list state_init state_end;;


machine_init bin_machine;;
run_machine bin_machine "101010";; (* true *)

machine_init bin_machine;;
run_machine bin_machine "hello";; (* false *)
