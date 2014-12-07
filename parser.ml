
#load "dynlink.cma";;
#load "camlp4o.cma";;
#load "myStream.cmo";;

#use "type.ml";;
#use "tokenize.ml" ;;
#use "pretty_printer.ml"



(* ==== Grammaire du jeu =====
 
   Jeu ::= ( "joueurs" Joueurlist ) ( "jeu" Couplist ) ( "pioche" Tuilelist ) ( "tour" int )
   
   Joueurlist ::= Joueur | Joueur Joueurlist
   
   Joueur ::= ( ident int Bool Coup )
  
   Bool ::= true | false

   Couplist ::= Coup Couplist | ε  
 
   Coup ::= ( Tuilelist )
   
   Tuilelist ::= Tuile Tuilelist | ε

   Tuile ::= Char | *

========================================================= *)

let (of_bool_token_list_to_token_Stream: bool * token list -> token Stream.t ) = fun t ->
  match t with 
    | (true, x ) -> Stream.of_list x
    | (false, _ ) -> failwith "Tokenize went wrong"
;;

let (parser_tuile : token Stream.t -> tuile ) =
  parser 
    | [< '(IdentMaj identmaj); stream >] -> identmaj
    | [< '(Smb "*") ; stream >] -> "*"
;;

let rec (parser_tuiles : token Stream.t -> tuiles ) =
  parser
    | [< t = parser_tuile ; ts =  parser_tuiles >] -> t::ts
    | [< >] -> []
;;

let (parser_coup : token Stream.t -> coup ) =
  parser
    | [< 'LPar ; ts = parser_tuiles ; 'RPar >] -> Coup(ts)
;;

let rec (parser_coups : token Stream.t -> coups ) =
  parser 
    | [< c = parser_coup ; cs = parser_coups >] -> c::cs
    | [< >] -> []
;;

let (parser_joueur : token Stream.t -> joueur ) =
  parser 
    | [< 'LPar ; '(IdentMaj identmaj); '(Int int) ; '(Bool bool) ; c = parser_coup ; 'RPar >] -> (identmaj,int,bool,c)
;; 

let rec (parser_joueurs : token Stream.t -> joueurs ) = 
  parser
   | [< j = parser_joueur ; js =  parser_joueurs >] -> j::js
   | [< >] -> []
;;

let (parser_jeu : token Stream.t -> jeu) = 
  parser
  | [< 'LPar ; '(Kwd "joueurs") ; js = parser_joueurs ;  'RPar ; 'LPar ; '(Kwd "jeu") ; cs = parser_coups ; 'RPar ; 'LPar ; '(Kwd "pioche") ; ts = parser_tuiles ; 'RPar ; 'LPar ; '(Kwd "tour") ; '(Int int) >] -> (js,cs,ts,int)
;;

(* TESTES *)

let x = test_tokenize "*";;
let y = of_bool_token_list_to_token_Stream x;;
let z =parser_tuile y;;
let _  = print_string (( pretty_tuile z) ^"\n");;
;;

let x = test_tokenize" C A B * R ";;
let y = of_bool_token_list_to_token_Stream x;;
let z = parser_tuiles y;;
let _  = print_string (( pretty_tuiles z) ^"\n");;
;;


let x = test_tokenize "( F A C I L E)";;
let y = of_bool_token_list_to_token_Stream x;;
let z = parser_coup y
let _  = print_string (( pretty_coup z) ^"\n");;
;;

let x = test_tokenize "( F A C I L E)
( C A * B R )";;
let y = of_bool_token_list_to_token_Stream x;;
let z =parser_coups y ;;
let _  = print_string (( pretty_coups z) ^"\n");;

let x = test_tokenize "(Pascal 17 true (S P O I N E * ))";;
let y = of_bool_token_list_to_token_Stream x;; 
let z = parser_joueur y
clet _  = print_string (( pretty_joueur z) ^"\n");;
;; 

let x = test_tokenize "(Pascal 17 true (S P O I N E * ))
(Laurent 42 true (N S A V))
(Marion 0 false (E E I N)))";;
let y = of_bool_token_list_to_token_Stream x;;
let z = parser_joueurs y
let _  = print_string (( pretty_joueurs z) ^"\n");;
;;

let x = test_tokenize "(joueurs
(Pascal 17 true (S P O I N E * ))
(Laurent 42 true (N S A V))
(Marion 0 false (E E I N)))
(jeu 
( F A C I L E)
( C A * B R ))
(pioche
C S N H I )
(tour 3)"
;;


let y = of_bool_token_list_to_token_Stream x;;

let z = parser_jeu y;;


let _  = print_string (( pretty_jeu z) ^"\n");;

