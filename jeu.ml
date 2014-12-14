open Lettres
open MultiEnsemble


module type TJeu = functor (Rule: REGLE) -> 
sig
    val coup_valide : Rule.combi list (* jeu en cours *) -> Rule.main (* main du joueur *)
      -> Rule.combi list (* nouveau jeu *) -> Rule.main (* nouvelle main du joueur *) -> bool (* a posé *) -> bool
    val initialiser : string list -> Rule.etat
  (*  val lit_coup : string -> Rule.main -> Rule.combi list -> bool -> (Rule.main * (Rule.combi list)) option
    val joue : Rule.etat -> (string * int) list*)
    val sauvegarde : Rule.etat -> string
  (*  val chargement : char Stream.t -> Rule.etat*)
end

(*
  type combi = t list
  type main = t MultiEnsemble.mset*)

module Jeu: TJeu = functor (Rule : REGLE) ->
    struct


(* A FINIR premier if *)
      let (coup_valide : Rule.combi list -> Rule.main -> Rule.combi list -> Rule.main -> bool -> bool ) = fun j m new_j new_m b ->
	if b then 
	  List.for_all (fun a -> a==true) (List.map (Rule.combi_valide) new_j) && true
	else 
	  let rec (pose_j : Rule.combi list -> Rule.combi list -> Rule.combi list ) = fun cl1 cl2 ->
	    match cl2 with
	      |t::q -> if (List.mem t cl1) 
		then pose_j cl1 q
		else t::(pose_j cl1 q)
	      |[]-> []	    
	  in
	  Rule.premier_coup_valide m (pose_j j new_j)
	  

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
  type etat = { noms: string array; scores: int array; mains: main array;
		table: combi list; pioche: main; pose: bool array; tour: int}

  type main = t MultiEnsemble.mset
*)

      let (sauvegarde : Rule.etat -> string ) = fun e ->
	let rec (affiche_main : Rule.t MultiEnsemble.mset -> string ) = fun m ->
	  match m with
	    |[] -> ""
	    |(x,0)::ms -> (affiche_main ms)
	    |(x,n)::ms -> (Rule.ecrit_valeur x) ^ " " ^ (affiche_main ((x,n-1)::ms))
	in
	let rec (joueurs : string -> int -> string ) = fun s i -> 
	  if (i = Array.length e.Rule.noms) then
	    s
	  else
	    let nom = e.Rule.noms.(i)
	    and score = string_of_int (e.Rule.scores.(i))
	    and b = string_of_bool (e.Rule.pose.(i))
	    and main = affiche_main (e.Rule.mains.(i))	      
	    in joueurs (s^"\n ("^nom^" "^score^" "^b^" ("^main^")"^")") (i+1)

	and (jeu : string -> Rule.combi list -> string)= fun s ta ->
	  match ta with
	    |[] -> s
	    |c::l -> let combi = List.fold_left (fun s t -> s^" "^(Rule.ecrit_valeur t)) "" c
		     in jeu (s^"\n ("^combi^")") l
	and pioche = "\n "^ affiche_main (e.Rule.pioche)
	in
	
	"(joueurs" ^ (joueurs "" 0) ^ ") \n"^"(jeu" ^ (jeu "" e.Rule.table)^") \n" ^ "(pioche"^pioche^") \n" ^ "(tour " ^ (string_of_int e.Rule.tour) ^ ")\n"
	*)
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

*)
