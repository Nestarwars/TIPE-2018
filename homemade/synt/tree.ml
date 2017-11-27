

#open "homemade_lexer";; (* change to true name *)

type 'a btree =
  | Empty
  | N of 'a btree * 'a * 'a btree
;;

type lexTree == lexeme btree ;;


