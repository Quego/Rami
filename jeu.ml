open Lettres
open MultiEnsemble
open Tokenize
open Parser

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


module Jeu: TJeu = functor (Rule : REGLE) ->
    struct

      let (coup_valide : Rule.combi list -> Rule.main -> Rule.combi list -> Rule.main -> bool -> bool ) = fun j m new_j new_m b ->
	if b then 
	  let rec (jouer_main : Rule.main -> Rule.main -> Rule.main ) = fun ma new_ma ->
	    match new_ma with
	      |x::xs -> jouer_main (MultiEnsemble.supp x ma) xs
	      |[] -> ma
	  and ( combi_list_to_list : Rule.combi list  -> Rule.t list ) = fun cl  ->
	    match cl with
	      |[] -> []
	      |x::xs -> match x with
		  |[] -> combi_list_to_list xs
		  |w::ws -> w::(combi_list_to_list ((List.tl x)::xs))
	  and ( new_multi_ens : Rule.t list -> Rule.main -> Rule.main ) = fun tl m->
	    match tl with
	      |[] -> m
	      |x::xs -> new_multi_ens xs (MultiEnsemble.add x m)
	      in		
	  List.for_all (fun a -> a==true) (List.map (Rule.combi_valide) new_j) && 
	    let a = combi_list_to_list new_j and b = combi_list_to_list j in
	    let ax = (new_multi_ens a []) and bx = (new_multi_ens b []) in
	    MultiEnsemble.egal ax  (MultiEnsemble.union bx (jouer_main m new_m))
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
	
      ;;
(*
 let rec tuiles f =
    let rec lireVal = parser
      |[<'Analex.RPar>] -> []
      |[<'Analex.TGen(s);f>] -> (Analex.TGen(s))::(lireVal f) in
    match f with parser
      |[<'Analex.TGen(s);l=tuiles>] ->
	(Rule.lit_valeur ([Analex.TGen(s)]))::l
      |[<'Analex.LPar;t=lireVal;l=tuiles>] ->
	(Rule.lit_valeur t)::l
      |[<>] -> []
 ;;
*)
 let (parser_combi : token Stream.t -> Rule.t) =
   parser 
     | [< '(IdentMaj identmaj); _ >] -> (Rule.lit_valeur [IdentMaj identmaj])
     | [< '(Smb "*") ; _ >] -> (Rule.lit_valeur [Smb "*"])
 ;;

 let rec (parser_combis : token Stream.t -> Rule.combi  ) =
   parser
     | [< t = parser_combi ; ts =  parser_combis >] -> t::ts
     | [< >] -> []
 ;;


      let (lit_coup :  string -> Rule.main -> Rule.combi list -> bool -> (Rule.main * (Rule.combi list)) option) = fun joueur m jeu b ->
	print_string (joueur ^ " à vous de jouer");
	let i = ref 0
	and res = ref []
	and new_m = ref [] in 
	while (!i>2 && !i<1) do
	  print_string "1 pour jouer, 2 pour piocher \n";
	  i:= read_int ()
	done;
	if (!i == 1) then
	  begin
	    let coupvalide = ref false in
	    while not(!coupvalide) do
	      print_string "Combien de combi y a t'il sur le nouveau jeu?";
	      let i = ref (read_int()) in
	      print_string "Entrez le nouveau jeu\n";
	      let new_j = ref [] in
	      while (!i<> 0) do	
		let s = read_line () in 
		new_j := (parser_combis(tokenizer(Stream.of_string s)))::(!new_j);	
		i := !i -1;
	      done;
	      print_string "Entrez votre nouvelle main:\n";
	      let sa = (read_line()) in
	      new_m := List.fold_right (MultiEnsemble.add) (parser_combis(tokenizer(Stream.of_string sa))) MultiEnsemble.vide; 
	      if coup_valide jeu m !new_j !new_m b then
		
		
		let res = ref [] in
		
		begin
		  if b then 
		    res:= !new_j
		  else 
		    res:=List.filter (fun a -> not(List.mem a jeu))!new_j;
		  coupvalide:=true
		end;
	    done;
	    Some(!new_m,!res)
	  end

	else
	  None
      ;;
      

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
