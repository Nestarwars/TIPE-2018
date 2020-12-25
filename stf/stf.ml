#open "sys";;

  type arbre = 
     |Vide
     |N of arbre * string * arbre
  ;;
    

  let rec string_of_arbre a = match a with
  |Vide -> "_"
  |N(g,x,d) ->"N(" ^ string_of_arbre g ^" "^ x^" "^string_of_arbre d ^ ")"
;;
  
  

(* noFileErr renvoie une erreur avec un message en cas de défaut du fichier d'entree *)
let noFileErr() =
  prerr_endline "Utilisation: stf <fichier-sortie> <fichier-entrée>";
  io__exit 1;
;;


(* récupération du nom de fichier en sortie depuis la ligne de commande *)
let getOutName( argv ) =
  if( vect_length argv >= 2 )
  then( argv.(1) )
  else(
    noFileErr ();
  )
;;


(* récupération du nom de fichier en entrée depuis la ligne de commande *)
let getInName( argv ) =
  if( vect_length argv >= 3 )
  then( argv.(2) )
  else(
    noFileErr ();
  )
;;


(* Utilisation de l'éxécutable:
   
   ./stf fichier_sortie fichier_entrée
   
   Si pas de fichier spécifié en sortie/entrée, erreur+exit 1
*)

let ch_out = open_out (getOutName sys__command_line) in (* canal de sortie (fichier forth) *)
let ch_in = open_in (getInName sys__command_line) in    (* canal d'entrée (fichier scheme) *)
try
  let lexbuf = lexing__create_lexer_channel ch_in in (* analyse syntaxique *)
  while true do
    let result = parser__Main lexer__Token lexbuf in
    (* analyse lexicale : attrape le résultat en forth
       lexer__Token : fonction de reconaissance des tokens *)
    output_string ch_out result; (* envoi dans le fichier *)
  done;
with
| Eof ->
  flush ch_out;
  close_out ch_out;
  close_in  ch_in;
;;
