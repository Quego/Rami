open Lettres
open MultiEnsemble


module type TJeu = functor (Rule: REGLE) -> 
sig
    val coup_valide : Rule.combi list (* jeu en cours *) -> Rule.main (* main du joueur *)
      -> Rule.combi list (* nouveau jeu *) -> Rule.main (* nouvelle main du joueur *) -> bool (* a posé *) -> bool
    val initialiser : string list -> Rule.etat
  (*  val lit_coup : string -> Rule.main -> Rule.combi list -> bool -> (Rule.main * (Rule.combi list)) option
    val joue : Rule.etat -> (string * int) list
    val sauvegarde : Rule.etat -> string
    val chargement : char Stream.t -> Rule.etat*)
end

module Jeu: TJeu = functor (Rule : REGLE) ->
    struct

      let (coup_valide : Rule.combi list -> Rule.main -> Rule.combi list -> Rule.main -> bool -> bool ) = fun j m new_j new_m b ->
	if b then 
	  (List.map (Rule.combi_valide) new_j) 
	  
	else 
	  let (pose_j : combi list -> combi list -> combi list ) = fun cl1 cl2 ->
	    cl2 
	  in
	    match cl1 with 
	      |
	  Rule.premier_coup_valide m (pose_j j new_j)

val filter : ('a -> bool) -> 'a list -> 'a list

filter p l returns all the elements of the list l that satisfy the predicate p. The order of the elements in the input list is preserved.
	  
val mem : 'a -> 'a list -> bool

mem a l is true if and only if a is equal to an element of l.
	  

      let (initialiser : string list -> Rule.etat ) = fun sl ->
	let lg = List.length sl in 
	let main_array = Array.make lg  (MultiEnsemble.vide) 
	and name_array  = Array.of_list sl
	and scores_array = Array.make lg 0
	and table_list = []
	and pose_array = Array.make lg false
	and turn = 0
	and pioche_list = ref Rule.paquet in
	for i = 0 to lg-1 do
	  for j = 0 to Rule.main_initiale-1 do
	    let (x,r) = MultiEnsemble.rand !pioche_list in
	    main_array.(i) <- MultiEnsemble.add x main_array.(i);
	    pioche_list := r
	  done
	done;	   
	{Rule.noms=name_array;scores=scores_array;mains=main_array;table=table_list;pioche = !pioche_list;pose = pose_array; tour = turn}
      ;;


(*
      let (lit_coup :  string -> Rule.main -> Rule.combi list -> bool -> (Rule.main * (Rule.combi list)) option) = fun s m cl b ->
	if (bool == false) then
	  None
	else
	  *)
      

    end
;;



(*
type paquet = { cont : contenu; poids : int ; s : solidite }
;;

type inventaire = paquet list
;;

let pq1 = {cont=Meuble;poids=1;s=Fragile};;
let pq2 = {cont=Plante;poids=2;s=Fragile};;
let pq3 = {cont=Objet;poids=3;s=Robuste};;

let inv = [pq1;pq2;pq3];;
module type REGLE =
sig
  type t
  type combi = t list
  type main = t MultiEnsemble.mset
  type etat = { noms: string array; scores: int array; mains: main array;
		table: combi list; pioche: main; pose: bool array; tour: int}
  val paquet : t MultiEnsemble.mset
  val combi_valide : combi -> bool
  val premier_coup_valide : main (* main du joueur *) -> combi list (* pose du joueur *) -> main (* nouvelle main du joueur *) -> bool
  val points : combi list (* jeu en cours *) -> main (* main du joueur *) -> combi list (* nouveau jeu *) -> main (* nouvelle main du joueur *) -> int
  val points_finaux : main -> int
  val main_min : int
  val main_initiale : int
  val lit_valeur : token list -> t
  val ecrit_valeur : t -> string
  val fin_pioche_vide : bool
end
;;*)
