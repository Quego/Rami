#load "dynlink.cma" 
#load "camlp4o.cma"
#load "myStream.cmo"

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

  let (paquet : t MultiEnsemble.mset ) = [('A',8);('B',2);('C',3);('D',3);('E',16);('F',2);('G',2);('H',2);('I',9);('J',1);('K',1);('L',6);('M',4);('N',7);('O',7);('P',2);('Q',1);('R',7);('S',7);('T',7);('U',7);('V',2);('W',1);('X',1);('Y',1);('Z',1)]
  
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


(*Cas de deux Joker a gerer :Choix utilisateur ?*)
let rec (cpt_combi : combi -> int) = fun c ->
  match c with
    | [] -> 0
    | (T(i1,_))::(T(i2,_))::Joker::cs -> if (i1 == i2)
      then ((3*i1) + (cpt_combi cs))
      else (i1 + i2 + i2 + 1 ) + (cpt_combi cs)
    | (T(i1,_))::Joker::(T(i2,_))::cs -> if (i1 == i2) 
      then ((3*i1) + (cpt_combi cs))
      else (i1 + i1 + 1 + i2 ) + (cpt_combi cs)
    | Joker::(T(i1,_))::(T(i2,_))::cs -> if (i1 == i2) 
      then ((3*i1) + (cpt_combi cs))
      else (i1 -1 + i1 + i2 ) + (cpt_combi cs)
    | (T(i,_))::cs -> (i + (cpt_combi cs))
;;


let rec (add_list : int list -> int ) = fun l ->
  match l with
    | [] -> 0
    | x::xs -> x + (add_list xs)
;;


let (cpt_r : combi list -> int ) = fun cl ->
add_list (List.map cpt_combi cl)

let rec (combi_valide_couleur : combi -> bool) = fun c ->
match c with 
  | [] -> true
  | (T(_,_))::[] -> true
  | Joker::[] -> true
  | Joker::(T(x,c))::cs -> true && (combi_valide_couleur ((T(x,c))::cs))
  | (T(_,c1))::Joker::(T(x,c2))::cs -> if (c1 == c2) 
    then true && (combi_valide_couleur ((T(x,c2))::cs))
    else false
  | (T(x,c))::Joker::cs -> true && (combi_valide_couleur (Joker::cs))
  | (T(_,c1))::(T(x,c2))::cs -> if (c1 == c2) 
    then true && (combi_valide_couleur ((T(x,c2))::cs))
    else false
  | Joker::Joker::cs -> true && (combi_valide_couleur cs)
;;

let rec (combi_valide_cons : combi -> bool ) = fun c ->
match c with
  | [] -> true
  | (T(_,_))::[] -> true
  | Joker::[] -> true
  | Joker::(T(i,c))::cs -> true && (combi_valide_cons ((T(i,c))::cs))
  | (T(i1,_))::Joker::(T(i2,c))::cs -> if (i1 == (i2-2)) 
    then true && (combi_valide_cons ((T(i2,c))::cs))
    else false
  | (T(x,c))::Joker::cs -> true && (combi_valide_cons (Joker::cs))
  | (T(i1,_))::(T(i2,c))::cs -> if (i1 == (i2-1)) 
    then true && (combi_valide_cons ((T(i2,c))::cs))
    else false
  | Joker::Joker::cs -> true && (combi_valide_cons cs)
;;

let (combi_valide : combi -> bool ) = fun c ->
 (combi_valide_couleur c) || (combi_valide_cons c)

let rec (combis_valide : combi list-> bool ) = fun cl ->
  let l = List.map combi_valide cl
  in 
  let rec (combis_valide_aux : bool list-> bool ) = fun cla ->
  match cla with
    |[] -> true
    |x::xs -> x && (combis_valide_aux xs)
  in combis_valide_aux l
;;

module Rummikub = 
struct

  type couleur = 
    | Bleu
    | Rouge
    | Jaune
    | Noir
  ;;

  type t = 
      T of int*couleur | Joker
  ;;

  type combi = t list
  ;;

  type main = t MultiEnsemble.mset
  ;;

  type etat = { noms: string array; scores: int array; mains: main array;
		table: combi list; pioche: main; pose: bool array; tour: int}
  ;;

  let (paquet : t MultiEnsemble.mset ) = [(T(1,Bleu),2);(T(1,Rouge),2);(T(1,Jaune),2);(T(1,Noir),2);(T(2,Bleu),2);(T(2,Rouge),2);(T(2,Jaune),2);(T(2,Noir),2);(T(3,Bleu),2);(T(3,Rouge),2);(T(3,Jaune),2);(T(3,Noir),2);(T(4,Bleu),2);(T(4,Rouge),2);(T(4,Jaune),2);(T(4,Noir),2);(T(5,Bleu),2);(T(5,Rouge),2);(T(5,Jaune),2);(T(5,Noir),2);(T(6,Bleu),2);(T(6,Rouge),2);(T(6,Jaune),2);(T(6,Noir),2);(T(7,Bleu),2);(T(7,Rouge),2);(T(7,Jaune),2);(T(7,Noir),2);(T(8,Bleu),2);(T(8,Rouge),2);(T(8,Jaune),2);(T(8,Noir),2);(T(9,Bleu),2);(T(9,Rouge),2);(T(9,Jaune),2);(T(9,Noir),2);(T(10,Bleu),2);(T(10,Rouge),2);(T(10,Jaune),2);(T(10,Noir),2);(T(11,Bleu),2);(T(11,Rouge),2);(T(11,Jaune),2);(T(11,Noir),2);(T(12,Bleu),2);(T(12,Rouge),2);(T(12,Jaune),2);(T(12,Noir),2);(T(13,Bleu),2);(T(13,Rouge),2);(T(13,Jaune),2);(T(13,Noir),2);(Joker,2)]
  ;;

(* Tester si les combi sont dans la main *)
  let (premier_coup_valide : main -> combi list -> bool) = fun m cl ->
    let cpt = cpt_r cl
    in
      if (cpt>30)
      then
	combis_valide cl
      else false
  ;;
    

  
  let (main_min : int ) = 7
  ;;
  
  let (main_initiale : int ) = 14
  ;;
  

end
;;

