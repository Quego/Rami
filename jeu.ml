#load "dynlink.cma" 
#load "camlp4o.cma"
#load "myStream.cmo";
#use "regle.ml";;

module TJeu = functor (Rule : REGLE) ->
    struct
(*
      let ( coup_valide : Rule.combi list -> Rule.main -> Rule.combi list -> Rule.main -> bool  -> bool ) = fun cl1 m1 cl2 m2 b ->*)
(*
      let (initialiser : string list -> Rule.etat ) = fun sl ->
(*	let (initialiser_aux : string list -> int -> Rule.etat ) = fun sla i ->*)
{noms=[|""|];scores=[||];mains=[||];table=[];pioche = [];pose = []; tour = 0}
      ;;*)

let (pq1 : Rule.etat) = {Rule.noms=[|"salut";"t"|];scores=[||];mains=[||];table=[];pioche = [];pose = [||]; tour = 0}
      ;;

    end 
;;

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
;;
