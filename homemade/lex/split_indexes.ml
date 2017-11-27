
(* split_indexes -> string -> char list
   split_indexes s cl returnes a list of the position of any element of cl in s.
*)
let split_indexes s schar =
  let slen = string_length s in

  let rec loop indexes i =
    if( i = slen-1 ) (* End of string *)
    then( indexes )
    else(
      let c = nth_char s i in
      if( mem c schar )
      then(
	loop (i::indexes) (i+1)
      )
      else(
	loop (indexes) (i+1)
      )
    )
  in
  loop [] 0
;;
  
