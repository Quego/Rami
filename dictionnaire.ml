module Dictionnaire =
  struct 

    type dico = Noeud of dico array * bool | Feuille

    let (dico_vide : dico ) = Noeud((Array.make 26 Feuille), false)

   (* let (member : string -> dico -> bool ) *)

    (*    let (add : string -> dico -> int) = fun s d ->
      let x = String.get s 0
       in let y = (Char.code x) - (Char.code 'a')
	  in Array.set d y (Noeud([|Feuille|], true))  *)


  end
;;

let x = Dictionnaire.dico_vide;;

String.get "salut" 0;; 

add 

let x = (Char.code 'b') - (Char.code 'a');;
