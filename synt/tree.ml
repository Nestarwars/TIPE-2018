

#open "homemade_lexer";;

type 'a btree =
  | Empty
  | Node of 'a btree * 'a * 'a btree
;;

type lexTree == lexeme btree ;;


