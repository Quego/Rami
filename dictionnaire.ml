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

    let rec (add : string -> dico -> dico) = fun s d ->
      if ((String.length s) == 0 ) then 
	match d with
	  |(Noeud(da,b)) -> (Noeud(da,true))
	  | Feuille -> Noeud((Array.make 26 Feuille), true)
      else 
	match d with
	  | (Noeud(da,b)) ->
	    let x = String.get s 0 
	    in let y = (Char.code x) - (Char.code 'a')
	       in let db =  (Array.get da y)
		  in let z = add (String.sub s 1 ((String.length s) -1)) db
		     in Array.set da y z;
		     Noeud(da,b);
	  | Feuille -> add (String.sub s 1 ((String.length s) -1)) dico_vide


  end
;;

String.sub "aaaa" 1 3;;

Array.set a n x modifies array a in place, replacing element number n with x. You can also write a.(n) <- x instead of Array.set a n x. 
let x = Dictionnaire.dico_vide;;
 
Noeud((Array.set da y ( add (String.sub s 1 ((String.length s) -1)) db)),b)

	    let x = String.get s 0 
	    in let y = (Char.code x) - (Char.code 'a')
	       in let db =  (Array.get da y)
		  in let w = (add (String.sub s 1 ((String.length s) -1)) db)
		     in let z = (Array.set da y w)
			in Noeud(z,b)
	  
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

Dictionnaire.member "aaa" x;; 

Dictionnaire.add "b" x;;

Dictionnaire.member "b" x;;
