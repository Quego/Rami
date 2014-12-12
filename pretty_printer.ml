open Type

(* PRETTY PRINTEUR*)
let (pretty_tuile : tuile -> string ) = fun t -> t 
;;

let rec (pretty_tuiles : tuiles -> string ) = fun ts ->
String.concat " " (List.map pretty_tuile ts)
;;

let (pretty_coup : coup -> string ) = fun (Coup(ts)) ->
"(" ^ (pretty_tuiles ts) ^ ")"
;;

let rec (pretty_coups : coups -> string ) = fun cs ->
String.concat " " (List.map pretty_coup cs)
;;

let (pretty_joueur : joueur -> string ) = fun (p,i,b,c) ->
"(" ^ p ^ " " ^  (Pervasives.string_of_int i) ^ " " ^ b ^ " " ^ (pretty_coup c) ^ ")"
;;

let rec (pretty_joueurs : joueurs -> string ) = fun js ->
String.concat " " (List.map pretty_joueur js)
;;

let (pretty_jeu : jeu -> string ) = fun (js,cs,ts,i) ->
"(joueurs " ^ (pretty_joueurs js) ^ ") (jeu " ^ (pretty_coups cs) ^ ") (pioche " ^ (pretty_tuiles ts) ^ ") (tour " ^ (Pervasives.string_of_int i) ^ ")"
;;

