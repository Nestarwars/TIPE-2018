
#open "lexeme";;

type 'a btree =
  | Empty
  | Node of 'a btree * 'a * 'a btree
;;

type lextree == lexeme btree ;;
