#load "dynlink.cma" 
#load "camlp4o.cma"
#load "myStream.cmo";

#use "tokenize.ml";;

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
	    let char_s = String.get s 0 
	    in let num_s = (Char.code char_s) - (Char.code 'a')
	       in let new_d =  (Array.get da num_s)
	       and _s = (String.sub s 1 ((String.length s) -1))
		  in member _s new_d
	  | Feuille -> false
;;

    let rec (add : string -> dico -> dico) = fun s d ->
      if ((String.length s) == 0 ) then 
	match d with
	  |(Noeud(da,_)) -> (Noeud(da,true))
	  | Feuille -> Noeud((Array.make 26 Feuille), true)
      else 
	match d with
	  | (Noeud(da,b)) ->
	    let char_s = String.get s 0 
	    in let num_s = (Char.code char_s) - (Char.code 'a')	  
	       in let new_d =  (Array.get da num_s)
	       and _s = (String.sub s 1 ((String.length s) -1))
		  in let d_rec = add _s new_d
		     in Array.set da num_s d_rec;
		     Noeud(da,b);
	  | Feuille -> 
	    let da = (Array.make 26 Feuille)
	    in let char_s = String.get s 0 
	       in let num_s = (Char.code char_s) - (Char.code 'a')	  
		  in let new_d =  (Array.get da num_s)
		  and _s = (String.sub s 1 ((String.length s) -1))
		     in let d_rec = add _s new_d
			in Array.set da num_s d_rec;
			Noeud(da,false);
    ;;

    let rec (remove : string -> dico -> dico) = fun s d ->
     if ((String.length s) == 0 ) then 
	match d with
	  |(Noeud(da,b)) -> (Noeud(da,false))
	  | Feuille -> dico_vide
     else 
       match d with
	 | (Noeud(da,b)) ->
	    let char_s = String.get s 0 
	    in let num_s = (Char.code char_s) - (Char.code 'a')	  
	       in let new_d =  (Array.get da num_s)
	       and _s = (String.sub s 1 ((String.length s) -1))
		  in let d_rec = remove _s new_d
		     in Array.set da num_s d_rec;
		     Noeud(da,b);
	  | Feuille -> d
    ;;


    let rec (of_stream : char Stream.t -> dico ) = fun cs ->
      let rec (parser_dico : token_dico Stream.t -> dico ) = fun t ->
	let rec (parser_dico_acc :  token_dico Stream.t -> dico -> dico ) = fun s acc ->
	  match s with parser
	    | [< '(Identd ident) ; td >] -> parser_dico_acc td (add ident acc)
	    | [< >] -> acc
	in parser_dico_acc t (dico_vide)
      in parser_dico (tokenize_dico cs)
    ;;

  end
;;


let x = Stream.of_string "coucou
comment
va
la"
;;
;;

let dico = Dictionnaire.of_stream x;;

let x = tokenize_dico x ;;

x;;

let rec (parser_dico : token_dico Stream.t -> Dictionnaire.dico ) = fun t ->
  let rec (parser_dico_acc :  token_dico Stream.t -> Dictionnaire.dico -> Dictionnaire.dico ) = fun s acc ->
    match s with parser
    | [< '(Identd ident) ; td >] -> parser_dico_acc td (Dictionnaire.add ident acc)
    | [< >] -> acc
  in parser_dico_acc t (Dictionnaire.dico_vide)
;;

let (of_bool_token_list_to_token_Stream: bool * token_dico list -> token_dico Stream.t ) = fun t ->
  match t with 
    | (true, x ) -> Stream.of_list x
    | (false, _ ) -> failwith "Tokenize went wrong"
;;

let x = of_bool_token_list_to_token_Stream x;;
let dico = parser_dico x;;

Dictionnaire.member "coucou" dico;;
Dictionnaire.member "comment" dico;;
Dictionnaire.member "va" dico;;
Dictionnaire.member "la" dico;;
Dictionnaire.member "vie" dico;;

let x = Dictionnaire.dico_vide;;
let y = Dictionnaire.dico_vide;;
Dictionnaire.member "aa" x;; 

#trace Dictionnaire.add;;
Dictionnaire.add "aab" x;;
Dictionnaire.add "d" x;;
Dictionnaire.add "zzzz" x;;

Dictionnaire.member "aab" x;;
Dictionnaire.member "zzz" x;;
Dictionnaire.remove "aab" x;;
Dictionnaire.remove "aa" x;;
Dictionnaire.member "aab" x;;


Dictionnaire.member "cde" x;;

Dictionnaire.add "abcd" x;;

Dictionnaire.remove "aab" x;;

Dictionnaire.member "aab" x;;

