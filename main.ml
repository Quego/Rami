open Dictionnaire
open MultiEnsemble
open Tokenize
open MyStream
open Pretty_printer
open Type
open Lettres
open Rummikub
open Jeu

module R = Jeu(Rummikub);;
module L = Jeu(Lettres);;

let (demandeP : int -> int ) = fun i ->
  print_string "Veuiller entrez les prenoms \n";
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


let play = 
  while true do
    print_string "Pour jouer au Rami entrez 1 , pour jouer au Rummikub entrez 2\n";
    let x = read_int() in
    if (x==1) then 
      begin
	print_string "Vous jouez au Rami\n";
	let i = ref 0 in
	while !i<>1 && !i<>2 do
	  print_string("Entrez 1 pour commencer une nouvelle partie, 2 pour charger la partie existante \n");
	  i:= read_int();
	done;
	if !i = 2 then
	  begin
	    let save = "Jeuencours" in 
	    L.joue (L.chargement(Stream.of_channel(open_in (save))))
	  end
	else
	  begin
	    print_string "A combien voulez vous jouer ? 7 joueurs maximum\n";
	    let nbJ = read_int()
	    in let nbP = nbJoueur nbJ
	       in let listP = addPrenom nbP
		  in let p = L.initialiser listP
		     in L.joue p
	  end
      end
    else 
      begin
	print_string "Vous jouez au Rummikub\n";
	let i = ref 0 in
	while !i<>1 && !i<>2 do
	  print_string("Entrez 1 pour commencer une nouvelle partie, 2 pour charger la partie existante \n");
	  i:= read_int();
	done;
	if !i = 2 then
	  begin
	    let save = "Jeuencours" in
	      L.joue (L.chargement(Stream.of_channel(open_in (save))))
	  end
	else
	  begin
       	    print_string "A combien voulez vous jouer ? 7 joueurs maximum\n";
	    let nbJ = read_int()
	    in let nbP = nbJoueur nbJ
	       in let listP = addPrenom nbP
		  in let p = R.initialiser listP
		     in R.joue p
	  end
      end
  done
;;

