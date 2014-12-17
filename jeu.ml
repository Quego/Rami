open Lettres
open MultiEnsemble
open Tokenize

module type TJeu = functor (Rule: REGLE) -> 
sig
    val coup_valide : Rule.combi list (* jeu en cours *) -> Rule.main (* main du joueur *)
      -> Rule.combi list (* nouveau jeu *) -> Rule.main (* nouvelle main du joueur *) -> bool (* a posé *) -> bool
    val initialiser : string list -> Rule.etat
    val lit_coup : string -> Rule.main -> Rule.combi list -> bool -> (Rule.main * (Rule.combi list)) option
    val joue : Rule.etat -> (string * int) list
    val sauvegarde : Rule.etat -> string
    val chargement : char Stream.t -> Rule.etat
end


module Jeu: TJeu = functor (Rule : REGLE) ->
    struct

(* ============================= COUP VALIDE ==================================*) 
   
      let (coup_valide : Rule.combi list -> Rule.main -> Rule.combi list -> Rule.main -> bool -> bool ) = fun j m new_j new_m b ->
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
	if b 
	then 		
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
	  (Rule.premier_coup_valide m (pose_j j new_j) new_m)(* && 
	    let a = combi_list_to_list new_j and b = combi_list_to_list j in
	    let ax = (new_multi_ens a []) and bx = (new_multi_ens b []) in
	    MultiEnsemble.egal ax  (MultiEnsemble.union bx (jouer_main m new_m))*)
	  
   

(* ============================= INITIALISER ==================================*) 

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

(* ============================= SAUVEGARDE ==================================*) 

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
	    |c::l -> let combi = List.fold_left (fun s t -> s^(Rule.ecrit_valeur t)^" ") "" c
		     in jeu (s^"\n ("^combi^")") l
	and pioche = "\n "^ affiche_main (e.Rule.pioche)
	in
	
	"(joueurs" ^ (joueurs "" 0) ^ ") \n"^"(jeu" ^ (jeu "" e.Rule.table)^") \n" ^ "(pioche"^pioche^") \n" ^ "(tour " ^ (string_of_int e.Rule.tour) ^ ")\n"
	
      ;;

(* ========================== AFFICHE JEU ============================= *)


      let (affichage : Rule.etat -> string ) = fun e ->
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
	    in joueurs (s^"\n "^nom^" |Score : "^score^" |A déjà joué : "^b^" |Main : "^main^" ") (i+1)

	and (jeu : string -> Rule.combi list -> string)= fun s ta ->
	  match ta with
	    |[] -> s
	    |c::l -> let combi = List.fold_left (fun s t -> s^(Rule.ecrit_valeur t)^" ") "" c
		     in jeu (s^"\n ("^combi^")") l
	and pioche = "\n "^ affiche_main (e.Rule.pioche)
	in
	
	"Joueurs :" ^ (joueurs "" 0) ^ " \n"^"Table :" ^ (jeu "" e.Rule.table)^"\n" ^ "Pioche : "^pioche^") \n" ^ "Tour n° : " ^ (string_of_int e.Rule.tour) ^ "\n"
	
      ;;

(* ============================= LIT COUP ==================================*) 

      let (parser_combi_cont : token Stream.t -> string -> Rule.t ) = fun t identmaj ->
	match t with parser 
	  | [<  'LPar ; '(Int int) ; '(Other o) ; '(IdentMaj identmaj ) ; 'RPar >] -> Rule.lit_valeur [IdentMaj "T";LPar;Int int;Other o;IdentMaj identmaj;RPar] 
	  | [< >] -> Rule.lit_valeur [IdentMaj identmaj]

      let (parser_combi : token Stream.t -> Rule.t ) =
	parser 
	  | [< '(IdentMaj identmaj); s >] -> parser_combi_cont s identmaj 
	  | [< '(Smb "*") >] -> Rule.lit_valeur [Smb "*"]
      ;;
 

      let rec (parser_combis : token Stream.t -> Rule.t list ) =
	parser
	  | [< t = parser_combi ; ts =  parser_combis >] -> t::ts
	  | [< >] -> []

      ;;

      let (lit_coup :  string -> Rule.main -> Rule.combi list -> bool -> (Rule.main * (Rule.combi list)) option) = fun joueur m jeu b ->
	print_string "==============================================================\n";
	print_string (joueur ^ " à vous de jouer \n");
	print_string "==============================================================\n\n";
	let i = ref 0
	and c = ref 0
	and res = ref []
	and new_j = ref []
	and new_m = ref [] in 
	while (!i>2 || !i<1) do (* Choix Pioche / Joue *)
	  print_string "==============================================================\n";
	  print_string "1 :  Poser  ;  2 : Piocher \n";
	  print_string "==============================================================\n\n";
	  try
	    i:= read_int();
	  with |Failure "int_of_string" -> i:=0
	done;
	if (!i == 1) then
	  begin
	    let coupvalide = ref false in
	    while not(!coupvalide) do (* Récupération du nombre de combinaison et du jeu / main tant que ce n'est pas valide *)
	      print_string "==============================================================\n";
	      print_string "Nombre de combinaisons sur le nouveau jeu?\n";
	      print_string "==============================================================\n\n";
	      	while (!c<=0) do
		  try
		    c:= read_int();
		  with |Failure "int_of_string" -> c:=0
		done;
		new_j := [];
		print_string "==============================================================\n";
		print_string "Entrez le nouveau jeu\n";
		print_string "==============================================================\n\n";
	      while (!c>0) do	
		let s = read_line () in 
		try
		  new_j := (parser_combis(tokenizer(Stream.of_string s)))::(!new_j);
		  c := !c-1;
		with | Failure("Mauvaise combi") -> new_j:=[]
	      done;
	      c := 0;
	      print_string "==============================================================\n";
	      print_string "Entrez votre nouvelle main:\n";
	      print_string "==============================================================\n\n";	      
	      let sa = (read_line()) in
	      while (!new_m = []) do
		try
		  new_m := List.fold_right (MultiEnsemble.add) (parser_combis(tokenizer(Stream.of_string sa))) MultiEnsemble.vide; 
		with | Failure("Mauvaise combi") -> new_m :=[]
	      done;
	      if coup_valide jeu m !new_j !new_m b then	
		begin
		  if b then 
		    res:= !new_j
		  else 
		    res:=List.filter (fun a -> not(List.mem a jeu)) !new_j;
		  coupvalide:=true
		end;
	    done;
	    Some(!new_m,!res)
	  end
	else
	  None
      ;;
      
(* ============================= JOUE ==================================*) 

      let rec ( joue : Rule.etat -> (string * int) list ) = fun e ->
	print_string("\n");
	print_string(affichage e);
	print_string("\n");
	let i = ref 0 in 
	while !i<1 || !i>3 
	do
	  print_string "==============================================================\n";
	  print_string("1 : Jouer ;  2 : Sauvegarder, 3  : Quitter \n");
	  print_string "==============================================================\n\n";
	  try
	    i:= read_int();
	  with |Failure "int_of_string" -> i:=0
	done;
	match !i with
	  |1 -> (match lit_coup (e.Rule.noms.(e.Rule.tour)) (e.Rule.mains.(e.Rule.tour)) (e.Rule.table) (e.Rule.pose.(e.Rule.tour)) with
	      |None ->
		if e.Rule.pioche <> MultiEnsemble.vide 
		then
		  begin
		    let (t,l) = MultiEnsemble.rand (e.Rule.pioche) in
		    e.Rule.mains.(e.Rule.tour)<- MultiEnsemble.add t (e.Rule.mains.(e.Rule.tour));
		    joue({Rule.noms=e.Rule.noms;scores=e.Rule.scores;mains=e.Rule.mains;table=e.Rule.table;pioche=l;pose=e.Rule.pose;tour=(e.Rule.tour+1) mod (Array.length (e.Rule.noms))})
		  end
		else
		  begin
		    print_string "==============================================================\n";
		    print_string("Pioche vide, vous passez votre tour. \n");
		    print_string "==============================================================\n\n";
		    joue(e)
		  end
	      |Some(new_m,combis) when (e.Rule.pose.(e.Rule.tour)) ->
		begin
		  e.Rule.scores.(e.Rule.tour) <- e.Rule.scores.(e.Rule.tour) + Rule.points (e.Rule.table) (e.Rule.mains.(e.Rule.tour)) (combis) (new_m);
		  e.Rule.pose.(e.Rule.tour)<- true; 
		  if new_m = MultiEnsemble.vide && (not(Rule.fin_pioche_vide) || e.Rule.pioche=[]) 
		  then
		    begin
		      for i=0 to (Array.length e.Rule.noms) 
		      do
			e.Rule.scores.(i) <- Rule.points_finaux (e.Rule.mains.(i));
		      done;
		      List.combine(Array.to_list (e.Rule.noms))(Array.to_list (e.Rule.scores))
		    end
		  else
		    begin
		      let rec (remplir_main : Rule.main -> Rule.main -> int -> Rule.main * Rule.main ) =  fun main pioche i ->
			if i<1 then (main,pioche)
			else
			  let (t,l) = MultiEnsemble.rand (e.Rule.pioche) in
			  remplir_main (MultiEnsemble.add t main) l (i-1) 
		      in let (m,p)= remplir_main new_m (e.Rule.pioche) (Rule.main_min - (MultiEnsemble.taille new_m)) 
			 in e.Rule.mains.(e.Rule.tour)<- m;
			 joue({Rule.noms=e.Rule.noms;scores=e.Rule.scores;mains=e.Rule.mains;table=combis;pioche=p;pose=e.Rule.pose;tour=(e.Rule.tour+1) mod (Array.length (e.Rule.noms))})
		    end
		end
	      |Some(new_m,combis) ->
		begin
		  e.Rule.scores.(e.Rule.tour)<- e.Rule.scores.(e.Rule.tour) + Rule.points (e.Rule.table) (e.Rule.mains.(e.Rule.tour)) (combis@(e.Rule.table)) (new_m);
		  e.Rule.pose.(e.Rule.tour)<- true; 
		  if new_m = MultiEnsemble.vide && (not(Rule.fin_pioche_vide) || e.Rule.pioche=[]) 
		  then
		    begin
		      for i=0 to (Array.length e.Rule.noms) 
		      do
			e.Rule.scores.(i) <- Rule.points_finaux (e.Rule.mains.(i));
		      done;
		      List.combine(Array.to_list (e.Rule.noms))(Array.to_list (e.Rule.scores))	
		    end
		  else 
		    begin
		      let rec (remplir_main : Rule.main -> Rule.main -> int -> Rule.main * Rule.main ) =  fun main pioche i ->
			if i<1 then (main,pioche)
			else
			  let (t,l) = MultiEnsemble.rand (e.Rule.pioche)
			  in remplir_main (MultiEnsemble.add t main) l (i-1) 
		      in let (m,p)= remplir_main new_m (e.Rule.pioche) (Rule.main_min - (MultiEnsemble.taille new_m)) 
			 in e.Rule.mains.(e.Rule.tour)<- m;
			 joue({Rule.noms=e.Rule.noms;scores=e.Rule.scores;mains=e.Rule.mains;table=combis@(e.Rule.table);pioche=p;pose=e.Rule.pose;tour=(e.Rule.tour+1) mod (Array.length (e.Rule.noms))})
		    end
		end)
	    
	  |2 ->
	    begin
	      let save = "Jeuencours" 
	      in let out_channel = open_out  save 
		 in output_string out_channel (sauvegarde e);
		 close_out out_channel;
		 print_string "==============================================================\n";
		 print_string("Partie sauvegardé");
		 print_string "==============================================================\n";
		 joue e;
	    end
	  |_ -> []
      ;;

(* ============================= CHARGEMENT ==================================*)

      let (list_to_mset) = fun l ->
	List.fold_right MultiEnsemble.add l MultiEnsemble.vide
      ;;

      let rec (to_list_1 : ( string * int * bool * Rule.t list ) list  -> string list ) = fun l ->
	match l with
	      |[] -> []
	      |(x,_,_,_)::ls -> x::(to_list_1 ls )
      ;;

     let rec (to_list_2 : ( string * int * bool * Rule.t list ) list  -> int list ) = fun l ->
	   match l with
	      |[] -> []
	      |(_,x,_,_)::ls -> x::(to_list_2 ls )
     ;;

     let rec (to_list_3 : ( string * int * bool * Rule.t list ) list  -> bool list ) = fun l ->
	   match l with
	      |[] -> []
	      |(_,_,x,_)::ls -> x::(to_list_3 ls )
     ;;

     let rec (to_list_4 : ( string * int * bool * Rule.t list ) list  -> Rule.t list list ) = fun l ->
	  match l with
	      |[] -> []
	      |(_,_,_,x)::ls -> x::(to_list_4 ls )

      ;;
     
(* =============== Grammaire du jeu ====================
 
   Jeu ::= ( "joueurs" Joueurlist ) ( "jeu" Couplist ) ( "pioche" Tuilelist ) ( "tour" int )
   
   Joueurlist ::= Joueur | Joueur Joueurlist
   
   Joueur ::= ( ident int Bool Coup )
  
   Bool ::= true | false

   Couplist ::= Coup Couplist |    
 
   Coup ::= ( Tuilelist )
   
   Tuilelist ::= Tuile Tuilelist |  

   Tuile ::= Char | * | T(int,string)

========================================================= *)

      let (parser_tuile_cont : token Stream.t -> string -> Rule.t ) = fun t identmaj ->
	match t with parser 
	  | [<  'LPar ; '(Int int) ; '(Other o) ; '(IdentMaj identmaj ) ; 'RPar >] -> Rule.lit_valeur [IdentMaj "T";LPar;Int int;Other o;IdentMaj identmaj;RPar] 
	  | [< >] -> Rule.lit_valeur [IdentMaj identmaj]

      let (parser_tuile : token Stream.t -> Rule.t ) =
	parser 
	  | [< '(IdentMaj identmaj); s >] -> parser_tuile_cont s identmaj 
	  | [< '(Smb "*") >] -> Rule.lit_valeur [Smb "*"]
      ;;
 

      let rec (parser_tuiles : token Stream.t -> Rule.t list ) =
	parser
	  | [< t = parser_tuile ; ts =  parser_tuiles >] -> t::ts
	  | [< >] -> [] 
      ;;
      
      let (parser_coup : token Stream.t -> Rule.combi ) =
	parser
	  | [< 'LPar ; ts = parser_tuiles ; 'RPar >] -> ts
      ;;
      
      let (parser_main : token Stream.t -> Rule.t list ) =
	parser
	  | [< 'LPar ; ts = parser_tuiles ; 'RPar >] -> ts
      ;;
      
      let rec (parser_coups : token Stream.t -> Rule.combi list ) =
	parser 
	  | [< c = parser_coup ; cs = parser_coups >] -> c::cs
	  | [< >] -> []
      ;;

      let (parser_joueur_cont : token Stream.t -> string -> int -> string * int * bool * Rule.t list ) = fun s identmaj int  ->
	match s with parser
	  | [< '(Bool "true") ; c = parser_main ; 'RPar >] -> (identmaj,int,true,c)
	  | [< '(Bool "false") ; c = parser_main ; 'RPar >] -> (identmaj,int,false,c)
      ;;
      
      let (parser_joueur : token Stream.t -> string * int * bool * Rule.t list ) =
	parser 
	  | [< 'LPar ; '(IdentMaj identmaj); '(Int int) ; stream >] -> parser_joueur_cont stream identmaj int
      ;;

      let rec (parser_joueurs : token Stream.t -> ( string * int * bool * Rule.t list ) list ) = 
	parser
	  | [< j = parser_joueur ; js =  parser_joueurs >] -> j::js
	  | [< >] -> []
      ;;
      
      let (parser_jeu : token Stream.t -> Rule.etat) = 
	parser
	  | [< 'LPar ; '(Kwd "joueurs") ; js = parser_joueurs ;  'RPar ; 'LPar ; '(Kwd "jeu") ; cs = parser_coups ;  'RPar ; 'LPar ; '(Kwd "pioche") ; ts = parser_tuiles ; 'RPar ; 'LPar ; '(Kwd "tour") ; '(Int int) >] ->  {Rule.noms = Array.of_list (to_list_1 js );																		 scores= Array.of_list (to_list_2 js );
				   mains=Array.of_list  ( (List.map list_to_mset (to_list_4 js )));
				   table=cs;
				   pioche =(list_to_mset ts);
				   pose = Array.of_list (to_list_3 js );
				   tour=int}
	  | [< >] -> failwith "ICI 6"
;;


      let ( chargement : char Stream.t -> Rule.etat ) = fun cs ->
	let ts = Tokenize.tokenizer cs in
	parser_jeu ts
      ;;

    end
;;

