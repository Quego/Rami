open MultiEnsemble
open Dictionnaire
open Complements
open Tokenize

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



module type REGLE  =
sig
  type t
  type combi = t list
  type main = t MultiEnsemble.mset
  type etat = { noms: string array; scores: int array; mains: main array;
		table: combi list; pioche: main; pose: bool array; tour: int}
  val paquet : t MultiEnsemble.mset
  val combi_valide : combi -> bool
  val premier_coup_valide : main (* main du joueur *) -> combi list (* pose du joueur *) (* -> main  nouvelle main du joueur *) -> bool
  val points : combi list (* jeu en cours *) -> main (* main du joueur *) -> combi list (* nouveau jeu *) -> main (* nouvelle main du joueur *) -> int
  val points_finaux : main -> int 
  val main_min : int
  val main_initiale : int
  val lit_valeur : token list -> t
  val ecrit_valeur : t -> string
  val fin_pioche_vide : bool
end



module Lettres : REGLE with type t=char =
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

  let (points : combi list -> main -> combi list -> main -> int) = fun j m new_j new_m ->
    let rec ( motPlusLong  : combi list -> combi list -> int ) = fun cl1 cl2 ->
match cl2 with
  |[] -> 0
  | t::q -> if (List.mem t cl1)
    then motPlusLong cl1 q
    else max (List.length t) (motPlusLong cl1 q)
    in
    let p = (List.length m) - (List.length new_m)
    in if ((List.length new_m) == 0 ) 
      then 2*p + (motPlusLong j new_j) 
      else p + (motPlusLong j new_j)
    


  let (points_finaux : main -> int ) = fun m -> 0
  ;; 
  
  let (main_min : int ) = 7
  ;;
  
  let (main_initiale : int ) = 14
  ;;

    
  let (lit_valeur : token list-> t ) = fun t ->
    match t with
      |[Tokenize.IdentMaj s] -> (String.get s 0)
      |[Tokenize.Smb x] -> (String.get x 0)
      | _ -> failwith "Cas a traiter"
  ;;

  let (ecrit_valeur: t -> string ) = fun x ->
    String.make 1 x 
  ;;

  let (fin_pioche_vide : bool ) = true
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
