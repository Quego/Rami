(*#load "dynlink.cma" 
#load "camlp4o.cma"
#load "myStream.cmo"*)

#use "multiensemble.ml";;
#use "dictionnaire.ml";;
#use "regle.mli";;

let (implode : char list -> string ) = fun l ->
  let res = String.create (List.length l) in
  let rec imp i = function
  | [] -> res
  | c :: l -> res.[i] <- c; imp (i + 1) l in
  imp 0 l;;

let rec(cpt_c : 'a list -> int ) = fun cl ->
  match cl with
    | t::q -> (List.length t) + (cpt_c q)
    | [] -> 0
;;


let dico = Dictionnaire.dico_vide;;


module Lettres =
struct
  
  type t = char
  ;;
  type combi = t list
  ;;
  type main = t MultiEnsemble.mset
  ;; 
  type etat = { noms: string array; scores: int array; mains: main array;
		table: combi list; pioche: main; pose: bool array; tour: int}
  ;;
  
  let (combi_valide : combi -> bool ) = fun c -> 
   ( (List.length c >= 3) && (Dictionnaire.member (implode c) dico)) 
  ;;

 

  let rec (premier_coup_valide : main -> combi list -> bool ) = fun m cl ->
    let cpt = cpt_c cl
    in if (cpt >= 6) then
	let rec (premier_coup_valide_cont : main -> combi list  -> bool ) = fun m cl ->
	  let rec (premier_coup_valide_aux : main -> combi  -> bool ) = fun ma cla ->
	    match cla with 
	      | t::q -> (MultiEnsemble.appartient t ma) && (premier_coup_valide_aux ma q)
	      | [] -> true
	  in
	  match cl with 
	    | t::q -> (combi_valide t) && (premier_coup_valide_aux m t) && (premier_coup_valide_cont m q)
	    | [] -> true
	in premier_coup_valide_cont m cl
      else false
  ;;
(*
  let (points : combi list -> main -> int) 
  ;;

  let (points_finaux : main -> int ) = fun m ->
*)  
  
  let (main_min : int ) = 7
  ;;
  
  let (main_initiale : int ) = 14
  ;;
  end 
;;
(*
premier_coup_valide_aux ['b';'c';'d'] ['a';'b';'c'];;

cpt_c [['a';'b';'c'];['d';'e';'f']]
let dico = Dictionnaire.dico_vide;;

Lettres.combi_valide ['a';'b';'c'];;
Lettres.combi_valide ['d';'e';'f'];;

let dico = Dictionnaire.add "abc" dico;;
let dico = Dictionnaire.add "def" dico;;
Lettres.premier_coup_valide ['a';'b';'c';'d';'e';'q';'f'] [['a';'b';'c'];['d';'e';'f']];;

*)
(*
module Rummikub = 
struct

  type couleur = 
    | Bleu
    | Rouge
    | Jaune
    | Noir
  ;;

  type t = 
     int*couleur
  ;;

  type main = t MultiEnsemble.mset
  ;;

  type etat = { noms: string array; scores: int array; mains: main array;
		table: combi list; pioche: main; pose: bool array; tour: int}
  ;;

  let (main_min : int ) = 7
  ;;
  
  let (main_initiale : int ) = 14
  ;;
  

end
;;
*)
