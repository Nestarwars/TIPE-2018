
type atom == char;; (* text atom == letter *)

type stateName == string;;
type trans == atom*stateName;; (* Transition: "this atom leads to this state". *)

type state == stateName*(trans list);; (* Basically a graph node. *)

type stateMachine = {
  Q: state list; (* Machine state list *)
  q: state;      (* Machine initial state *)
  F: state list; (* Machine end states list *)
  emptywd: bool;   (* Temp? is empty word accepted by the machine *)
  mutable act: state; (* Actual machine state *)
};;

let emptyStateList () = ([]:state list);;
let emptyState () = ( (("":stateName), ([]:trans list)) : state);;

let make_machine_empty () =
  {
    Q = emptyStateList ();
    q = emptyState ();
    F = emptyStateList ();
    emptywd = false;
    act = emptyState ();
  }
;;

(* make_machine: state list -> state -> state list -> bool -> stateMachine
   Creates a stateMachine with specified parameters.
 *)
let make_machine (stateList:state list) (initialState:state) (endStates:state list) accept_empty =
  {
    Q=stateList;
    q=initialState;
    F=endStates;
    emptywd = accept_empty;
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

let makeEmptyTransList () =
  ([]: trans list)
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

(* run_machine: stateMachine -> string -> bool*string
   Execute the machine consecutively on each character of the string.
   Return true if execution was successful, false otherwise, coupled with the parsed word in every case.
*)

let rec run_machine (m:stateMachine) s =

  let len = string_length s in
  let parsed_expr = ref "" in

  let rec run_aux i = (* s is non empty if called *)
    
    if( i < len )
    then(
      let at = ((nth_char s i): atom) in (* get actual analyzed char *)
      let err = step m at in (* process a step forward *)
      
      parsed_expr := !parsed_expr ^ (string_of_char at);

      if( not err )
      then(
	(* Machine continues. *)
	run_aux (i+1)
      )
      else(
	(* Machine stopped. *)
	false,!parsed_expr
      )
    )
    else( (* i=len *)
      (* We got from begining to end without error:
	 Is the node we are on an end node?
      *)
      (* FIXME wtf why need to apply 'not' here? wtf wtf wtf
	 magic
      *)
      not (mem m.act m.F),!parsed_expr
    )
  in

  if( len=0 )
  then(
    (* Empty string :"". Is the machine ok with empty word? *)
    m.emptywd,""(* booh we registered run_aux for nothing! *)
  )
  else(
    run_aux 0
  )
;;




(* Machine qui reconnait une suite quelconque de lettres d'un alphabet.
 *)
let alphabet_machine alphabet accept_empty =

  let trans_letter = make_transList_oneNode alphabet "state_letter" in

  let state_init = make_state "epsilon" trans_letter in
  let state_letter = make_state "state_letter" trans_letter in

  let state_list = [state_init;state_letter] in

  let alpha_machine = make_machine state_list state_init state_list accept_empty in
  alpha_machine
;;

(* Machine qui reconnait un entier naturel.
 *)
let natural_machine () =
  alphabet_machine [`0`;`1`;`2`;`3`;`4`;`5`;`6`;`7`;`8`;`9`] false
;;

(* Machine qui reconnait un mot précis.
 *)
let word_machine wd =
  let wdlen = string_length wd in
  if( wdlen = 0 )
  then( alphabet_machine [] true )

  else(
    let i = ref 0 in
    let state_eps = ref (emptyState ()) in
    let stlstEnd = ref (emptyStateList ()) in

    
    let rec wd_aux w stlst = (* word, state list *)

      let wlen = string_length w in

      if( wlen=0 )
      then(
	let fantoche_state = make_state ("state"^string_of_int !i) (makeEmptyTransList ())  in
	stlstEnd := [hd stlst]; (* Last state: front of the list. *)
	(* Went through the entire word: time to build our machine! *)
	make_machine ((*rev*) fantoche_state::stlst) !state_eps !stlstEnd false (* No empty since wd is notempty *)
      )
      else(
	(* Make next char state+trans *)
	let c = nth_char w 0 in
	let w2 = sub_string w 1 (wlen-1) in
	let nextStateName = "state"^string_of_int (!i+1) in
	let state = make_state ("state"^string_of_int !i) (make_transList [c] [nextStateName]) in
	if( !i=0 )then( state_eps := state  );
	i := !i+1;
	wd_aux w2 (state::stlst)
      )
    in

    wd_aux wd []
  )
;;

(* Machine qui reconnait un motif de m1 ou un motif de m2.
 *)
let branch_machine m1 m2 = ()
;;

(* star_machine, concat_machine...
   c'est à ce niveau là qu'il faut faire la disjonction ou dans la structure interne des machines? (plutot oui).
*)




(* Tests *)
(*
(* Machine states *)
let state_init = make_state "epsilon" (make_transList [`0`;`1`] ["q0";"q1"]);;
let state_0    = make_state "q0"      (make_transList [`0`;`1`] ["q0";"q1"]);;
let state_1    = make_state "q1"      (make_transList [`0`;`1`] ["q0";"q1"]);;
let state_list = [ state_init; state_0; state_1 ];;
let state_end  = [ state_0; state_1 ];; (* not used for now. *)

(* recognise binary string: (0?1?)* *)
let bin_machine = make_machine state_list state_init state_end false;;

let bin_machine_short = alphabet_machine [`0`;`1`] false;;

let wd_machine = word_machine "hello";;

machine_init bin_machine;;
run_machine bin_machine "101010";; (* true *)

machine_init bin_machine;;
run_machine bin_machine "hello";; (* false *)
*)

let is_bin s =
	let bin = alphabet_machine [`0`;`1`] false in
	run_machine bin s
;;

let is_nat s =
	run_machine (natural_machine ()) s
;;
