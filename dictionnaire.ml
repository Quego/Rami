open Tokenize

module type Dictionnaire =
  sig
    type dico = Noeud of dico array * bool | Feuille
    val dico_vide : dico
    val member : string -> dico -> bool
    val add : string -> dico -> dico
    val remove : string -> dico -> dico
    val of_stream : char Stream.t -> dico
    val to_list : dico -> string list
  end
;; 

module Dictionnaire =
  struct 

    type dico = Noeud of dico array * bool | Feuille
    ;;

    let (dico_vide : dico ) = Noeud((Array.make 26 Feuille), false)
    ;;

(* ================== MEMBER =================== *)

(* Retourne un booléen inquiquant la présence ou non d'un mot dans le dictionnaire 
Prends un string et un dictionnaire en entrée *)

    let rec (member : string -> dico -> bool)  = fun s d ->
      if ((String.length s) == 0 ) then 
	match d with
	  |(Noeud(_,b)) -> b (* Retourne le boolean correspondant la présence ou non du mot*)
	  | Feuille -> false			
      else 
	if ( (String.get s 0) == '*'  )
	then match d with
	  | (Noeud(da,_)) ->
	    let rec (member_joker : string -> dico -> int -> bool)  = fun s d i->    (* Applique sur toutes les branches du dictionnaire pour traiter tous les possible d' *)
	      let new_d =  (Array.get da i)
	      and _s = (String.sub s 1 ((String.length s) -1))
	      in  if (i>0) then
		  member _s new_d || member_joker s d (i-1)
		else  
		  member _s new_d
	    in member_joker s d 25
	  | Feuille -> false
	else
	  match d with
	    | (Noeud(da,_)) ->   (* Avance avec la prochaine lettre du mot *)
	      let char_s = String.get s 0 
	      in let num_s = (Char.code char_s) - (Char.code 'A')
		 in let new_d =  (Array.get da num_s)
		 and _s = (String.sub s 1 ((String.length s) -1))
		    in member _s new_d
	    | Feuille -> false (* Retourne faux si le mot ne continue pas *)
    ;;

(* ==================== ADD ===================== *)

(* Ajout un mot au dictionnaire 
Prends un string ( le mot ) et un dictionnaire et renvoie le dictionnaire avec le mot en plus *)

    let rec (add : string -> dico -> dico) = fun s d ->
      if ((String.length s) == 0 ) then 
	match d with
	  |(Noeud(da,_)) -> (Noeud(da,true)) (* Met le boolean de presence a vrai *)
	  | Feuille -> Noeud((Array.make 26 Feuille), true)
      else 
	match d with
	  | (Noeud(da,b)) ->
	    let char_s = String.get s 0 
	    in let num_s = (Char.code char_s) - (Char.code 'A')	  
	       in let new_d =  (Array.get da num_s)
	       and _s = (String.sub s 1 ((String.length s) -1))
		  in let d_rec = add _s new_d
		     in Array.set da num_s d_rec;
		     Noeud(da,b); (* Avance avec la prochaine lettre du mot *)
	  | Feuille -> 
	    let da = (Array.make 26 Feuille)
	    in let char_s = String.get s 0 
	       in let num_s = (Char.code char_s) - (Char.code 'A')	  
		  in let new_d =  (Array.get da num_s)
		  and _s = (String.sub s 1 ((String.length s) -1))
		     in let d_rec = add _s new_d
			in Array.set da num_s d_rec;
			Noeud(da,false); (* RCrée des nouvelles branche et avance avec la prochaine lettre du mot *)
    ;;

(* ==================== REMOVE ===================== *)

(* Surpprime un mot au dictionnaire 
Prends un string ( le mot ) et un dictionnaire et renvoie le dictionnaire avec le mot en moins  *)


    let rec (remove : string -> dico -> dico) = fun s d ->
     if ((String.length s) == 0 ) then 
	match d with
	  |(Noeud(da,_)) -> (Noeud(da,false)) (* Met le boolean de presence a faux *)
	  | Feuille -> dico_vide
     else 
       match d with
	 | (Noeud(da,b)) ->
	    let char_s = String.get s 0 
	    in let num_s = (Char.code char_s) - (Char.code 'A')	  
	       in let new_d =  (Array.get da num_s)
	       and _s = (String.sub s 1 ((String.length s) -1))
		  in let d_rec = remove _s new_d
		     in Array.set da num_s d_rec; (* Avance avec la prochaine lettre du mot *)
		     Noeud(da,b);
	  | Feuille -> d (* Le mot n'est pas présent donc on ne fait rien *)
    ;;

(* ==================== OF STREAM ============= *)

(* Crée un dictionnaire à partir d'un flux contenant un mot par ligne 
Prends un flux et renvoie le dictionnaire avec tous les mot du flux *)

    let (of_stream : char Stream.t -> dico ) = fun cs ->
      let (parser_dico : token_dico Stream.t -> dico ) = fun t ->
	let rec (parser_dico_acc :  token_dico Stream.t -> dico -> dico ) = fun s acc ->
	  match s with parser
	    | [< '(Identd ident) ; td >] -> parser_dico_acc td (add ident acc)
	    | [< >] -> acc
	in parser_dico_acc t (dico_vide)
      in parser_dico (tokenize_dico cs)
    ;;
(* ==================== TO LIST ============= *)

(* Convertit un dictionnaire en sa liste de mots
Prends un dictionnaire renvoie une liste de mot *)

    let (to_list : dico -> string list ) = fun d -> 
      let rec (to_list_aux : dico -> int -> string -> string list) = fun da i s ->
	if (i<0) then 
	  []
	else
	  match da with 
	    |(Noeud(db,false)) ->let char = Char.escaped (Pervasives.char_of_int ( i + Char.code 'A')) 
				 in let new_s = (s^char)
				    in let new_d =  (Array.get db i)
				       in (to_list_aux2 new_d 25 new_s) (* On concatène le mot avec la suite *)
	    |(Noeud(db,true)) ->let char = Char.escaped (Pervasives.char_of_int ( i + Char.code 'A'))
				in let new_s = (s^char)	  
				   in let new_d =  (Array.get db i)
				      in s::(to_list_aux2 new_d 25 new_s)(* On concatène le avec la suite et rajoute le mot dans la liste*)
	    |Feuille -> to_list_aux da (i-1) s

      and  (to_list_aux2 : dico -> int -> string -> string list)  = fun d2 j s2 ->  (* Parcours sur toutes les branches *)
	if (j>0) then
	  match d2 with
	    |Noeud(df,b) -> (to_list_aux d2 j s2)@(to_list_aux2 (Noeud(df,false)) (j-1) s2) (* Rapelle la fonction avec le boolean a false pour éviter les doublons *)
	    |Feuille ->  (to_list_aux d2 j s2)@(to_list_aux2 d2 (j-1) s2)
	else  
	  to_list_aux d2 j s2 
      in to_list_aux2 d 25 ("") 
      ;;

  end
;;


(* ============================= TESTES ============================ *)


(*

let d = Dictionnaire.dico_vide;;
Dictionnaire.add "A" ;;
Dictionnaire.member "A" d;;
Dictionnaire.member "B" d;;
Dictionnaire.remove "A" d;;
Dictionnaire.member "A" d;;
Dictionnaire.member "B" d;;

Dictionnaire.add "Z" ;;
Dictionnaire.member "Z" d;;
Dictionnaire.member "B" d;;
Dictionnaire.remove "Z" d;;
Dictionnaire.member "Z" d;;
Dictionnaire.member "B" d;;

Dictionnaire.add "" ;;
Dictionnaire.member "" d;;
Dictionnaire.member "B" d;;
Dictionnaire.remove "" d;;
Dictionnaire.member "" d;;
Dictionnaire.member "B" d;;

Dictionnaire.add "AZE" ;;
Dictionnaire.member "AZE" d;;
Dictionnaire.member "B" d;;
Dictionnaire.remove "AZE" d;;
Dictionnaire.member "AZE" d;;
Dictionnaire.member "B" d;;

let stream = Stream.of_string "HELLO
WORLD
THE 
IS 
SPARTA"
;;

let ds= Dictionnaire.of_stream stream;;

let ret = Dictionnaire.to_list ds ;;
*)
