
(* make_code: lextree -> string
   Construct and return the code described by the provided lextree.
 *)
let rec make_code (ltree:lextree) =

  (* Target language "makers" *)
  let forth_op op l r =
    ((make_code l)^" "^(make_code r)^" "^op^"\n") (* \n for user friendly output *)
  and forth_num nb =
    string_of_int nb
  and forth_var vb =
    vb (* This is not forth variable access. *)
  in

  (* Used language "makers" *)
  let op = forth_op
  and num = forth_num
  and var = forth_var
  in

  match
    ltree
  with
  | Empty -> ""
  | Node(l,PLUS,r) -> (op "+" l r)
  | Node(l,MINUS,r) -> (op "-" l r)
  | Node(l,TIMES,r) -> (op "*" l r)
  | Node(l,DIV,r) -> (op "/" l r)
  | Node(Empty,NUM(nb),Empty) -> (num nb)
  | Node(Empty,VAR(vb),Empty) -> (var vb)
  | _ -> failwith "Error in lextree structure"
;;
