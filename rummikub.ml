open MultiEnsemble
open Tokenize

module type REGLE  =
sig
  type t
  type combi = t list
  type main = t MultiEnsemble.mset
  type etat = { noms: string array; scores: int array; mains: main array;
		table: combi list; pioche: main; pose: bool array; tour: int}
  val paquet : t MultiEnsemble.mset
  val combi_valide : combi -> bool
  val premier_coup_valide : main (* main du joueur *) -> combi list (* pose du joueur *)  -> main (* nouvelle main du joueur *) -> bool
  val points : combi list (* jeu en cours *) -> main (* main du joueur *) -> combi list (* nouveau jeu *) -> main (* nouvelle main du joueur *) -> int
  val points_finaux : main -> int 
  val main_min : int
  val main_initiale : int
  val lit_valeur : token list -> t
  val ecrit_valeur : t -> string
  val fin_pioche_vide : bool 
end



module Rummikub : REGLE =
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



(*Cas de deux Joker a gerer :Choix utilisateur ?*)
  let rec (cpt_combi : combi -> int) = fun c ->
    match c with
      | [] -> 0
      | (T(i,_))::cs -> (i + (cpt_combi cs))
      | _ -> failwith "Unreacheable case"
      

  ;;


  let rec (add_list : int list -> int ) = fun l ->
    match l with
      | [] -> 0
      | x::xs -> x + (add_list xs)
  ;;

  let rec(joker_present : combi -> bool ) = fun c ->
match c with
  |Joker::_ -> false
  |_::cs -> true && joker_present cs
  |[] -> true
  ;;

  let rec(joker_presents : combi list -> bool )= fun cl ->
    List.for_all joker_present cl 
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



(* Tester si les combi sont dans la main *)
  let (premier_coup_valide : main -> combi list -> main -> bool) = fun m cl new_m ->
    if joker_presents cl 
    then false
    else
      let cpt = cpt_r cl
      in
      if (cpt>30)
      then
	combis_valide cl
      else false
  ;;
    
  let( points : combi list  -> main -> combi list  -> main  -> int ) = fun j m new_j new_m -> 0

  let rec (points_finaux : main -> int ) = fun m -> 
      match m with
	| [] -> 0
	|(T(i,c),n)::ms -> (i*n) + (points_finaux m)
	|(Joker,n)::ms -> (30*n) + (points_finaux m)
  ;;


  let (main_min : int ) = 7
  ;;
  
  let (main_initiale : int ) = 14
  ;;
  
  let (lit_couleur : string -> couleur) = fun s ->
    match s with
      |"Bleu" -> Bleu
      |"Rouge" -> Rouge
      |"Jaune" -> Jaune
      |"Noir" -> Noir
      |_ -> failwith "cas a traiter 1"
  ;;

  let (lit_valeur : token list -> t) = fun tl ->
    match tl with
      |[Tokenize.Smb x] -> Joker
      |[Tokenize.IdentMaj "T";Tokenize.LPar;Tokenize.Int i;Tokenize.Other ",";Tokenize.IdentMaj c;Tokenize.RPar] ->  T(i, lit_couleur c )
      | _ -> failwith "Mauvaise combi"
  ;;



  let (ecrit_valeur : t -> string ) = fun t ->
    let (ecrit_couleur : couleur -> string ) = fun c ->
      match c with
	|Bleu -> "Bleu"
	|Rouge -> "Rouge"
	|Jaune -> "Jaune"
	|Noir -> "Noir"
    in
    match t with
      |T(i,c) -> "T"^"("^(Pervasives.string_of_int i)^","^(ecrit_couleur c)^")"
      |Joker -> "*"
  ;;


  let fin_pioche_vide = true
  ;;

end
;;

