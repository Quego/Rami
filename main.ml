open Dictionnaire
open MultiEnsemble
open Tokenize
open MyStream
open Lettres
open Rummikub
open Jeu

module R = Jeu(Rummikub);;
module L = Jeu(Lettres);;


(* ============================== *)

(* fonctions récuperer le nombre de joueurs leurs prénoms et les ajouter dans une liste *)

let (demandeP : int -> int ) = fun i ->
  print_string "Entrez les prenoms \n";
  i;
;;

let rec (nbJoueur : int -> int) = fun i ->
  if (i>7 || i<0) then 
    let nbJ = read_int() in nbJoueur nbJ;
  else 
    demandeP i 
;;

let (addPrenom : int -> string list ) = fun i ->
 let rec (addPrenom_aux : int -> string list -> string list ) = fun ia acc ->
  match ia with
    |0 -> acc
    |_ -> let p = read_line()
	  in 
	  addPrenom_aux (ia-1) (p::acc)
 in 
 addPrenom_aux i []
;;

(* =========================== PLAY =================== *)

(* Fonction principale qui lance le jeu *)



let play =   
  let i = ref 0
  and x = ref 0
  and nbJ = ref 0 in
  print_string "==============================================================\n";
  print_string "================ RAMI/RUMMIKUB MACE/NOUGIUER =================\n";
  print_string "==============================================================\n\n";
  print_string "Pour jouer au Rami entrez 1 , pour jouer au Rummikub entrez 2\n";
  while (!x<>1 && !x<>2) do   (* Choix du type de partie : Rami/Rummikub *)
    try
      x:= read_int();
    with |Failure "int_of_string" -> x:=0
  done;
  if (!x==1) then  (* Si Rami choisi *)
    begin (* Choix de chargement ou nouvelle partie *)
      print_string "==============================================================\n";
      print_string "Vous jouez au Rami\n";
      print_string "==============================================================\n\n";
      while !i<>1 && !i<>2 do
	print_string "==============================================================\n";
	print_string("Entrez 1 pour commencer une nouvelle partie, 2 pour charger la partie existante \n");
	print_string "==============================================================\n\n";
	try
	  i:= read_int();
	with |Failure "int_of_string" -> i:=0
      done;
      if !i = 2 then (* Chargement de la partie *)
	begin
	  let save = "Jeuencours" in 
	  L.joue (L.chargement(Stream.of_channel(open_in (save))))
	end
      else
	begin (* Ecran de selection du nombres de joueurs/prenoms *)
	  print_string "==============================================================\n";
	  print_string "A combien voulez vous jouer ? 7 joueurs maximum\n";
	  print_string "==============================================================\n\n";
	  while (!nbJ<1 || !nbJ>7) do
	    try
	      nbJ:= read_int();
	    with |Failure "int_of_string" -> nbJ:=0
	  done;
	  print_string "==============================================================\n";
	  print_string "Nom des joueurs?\n";
	  print_string "==============================================================\n\n";
	  let listP = addPrenom !nbJ
	  in let p = L.initialiser listP
	     in (L.joue p) (* Lancement de la partie *) 
	end
    end
  else (* Si rummibuk choisi *)
    begin (* Choix de chargement ou nouvelle partie *)
      print_string "==============================================================\n";
      print_string "Vous jouez au Rummikub\n";
      print_string "==============================================================\n\n";
      while !i<>1 && !i<>2 do
 print_string "==============================================================\n";
	print_string("Entrez 1 pour commencer une nouvelle partie, 2 pour charger la partie existante \n");
	 print_string "==============================================================\n\n";
	try
	  i:= read_int();
	with |Failure "int_of_string" -> i:=0
      done;
      if !i = 2 then (* Chargement de la partie *)
	begin
	  let save = "Jeuencours" in 
	  R.joue (R.chargement(Stream.of_channel(open_in (save))))
	end
      else
	begin (* Ecran de selection du nombres de joueurs/prenoms *)
	   print_string "==============================================================\n";
       	  print_string "A combien voulez vous jouer ? 7 joueurs maximum\n";
	  print_string "==============================================================\n\n";
	  while (!nbJ<1 || !nbJ>7) do
	    try
	      nbJ:= read_int();
	    with |Failure "int_of_string" -> nbJ:=0
	  done;
	  print_string "Nom des joueurs?\n";
	  let listP = addPrenom !nbJ
	  in let p = R.initialiser listP
	     in (R.joue p) (* Lancement de la partie *) 
	end
    end 
;;
