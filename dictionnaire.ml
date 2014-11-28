module Dictionnaire =
  struct 

    type dico = Noeud of dico array * bool | Feuille

    let (dico_vide : dico ) = Noeud((Array.make 26 Feuille), false)

(* Cas avec * a rajouter *)
    let rec (member : string -> dico -> bool)  = fun s d ->
      if ((String.length s) == 0 ) then 
	match d with
	  |(Noeud(_,b)) -> b 
	  | Feuille -> false			
      else 
	match d with
	  | (Noeud(da,b)) ->
	    let x = String.get s 0 
	    in let y = (Char.code x) - (Char.code 'a')
	       in let db =  (Array.get da y)
		  in member (String.sub s 1 ((String.length s) -1)) db
	  | Feuille -> false

    (*    let (add : string -> dico -> int) = fun s d ->
      let x = String.get s 0
       in let y = (Char.code x) - (Char.code 'a')
	  in Array.set d y (Noeud([|Feuille|], true))  *)


  end
;;

String.sub "aaaa" 1 3;;


let x = Dictionnaire.dico_vide;;
 

let x =    Dictionnaire.Noeud
   ([|Dictionnaire.Noeud
   ([|Dictionnaire.Feuille; Dictionnaire.Feuille; Dictionnaire.Feuille;
      Dictionnaire.Feuille; Dictionnaire.Feuille; Dictionnaire.Feuille;
      Dictionnaire.Feuille; Dictionnaire.Feuille; Dictionnaire.Feuille;
      Dictionnaire.Feuille; Dictionnaire.Feuille; Dictionnaire.Feuille;
      Dictionnaire.Feuille; Dictionnaire.Feuille; Dictionnaire.Feuille;
      Dictionnaire.Feuille; Dictionnaire.Feuille; Dictionnaire.Feuille;
      Dictionnaire.Feuille; Dictionnaire.Feuille; Dictionnaire.Feuille;
      Dictionnaire.Feuille; Dictionnaire.Feuille; Dictionnaire.Feuille;
      Dictionnaire.Feuille; Dictionnaire.Feuille|],
   true); Dictionnaire.Feuille; Dictionnaire.Feuille;
      Dictionnaire.Feuille; Dictionnaire.Feuille; Dictionnaire.Feuille;
      Dictionnaire.Feuille; Dictionnaire.Feuille; Dictionnaire.Feuille;
      Dictionnaire.Feuille; Dictionnaire.Feuille; Dictionnaire.Feuille;
      Dictionnaire.Feuille; Dictionnaire.Feuille; Dictionnaire.Feuille;
      Dictionnaire.Feuille; Dictionnaire.Feuille; Dictionnaire.Feuille;
      Dictionnaire.Feuille; Dictionnaire.Feuille; Dictionnaire.Feuille;
      Dictionnaire.Feuille; Dictionnaire.Feuille; Dictionnaire.Feuille;
      Dictionnaire.Feuille; Dictionnaire.Feuille|],
   false);;

Dictionnaire.member "a" x;; 
