#load "dynlink.cma" 
#load "camlp4o.cma"
#load "myStream.cmo"

#use "multiensemble.ml"
#use "dictionnaire.ml";;

module Lettres =
  struct
 
    type t = char
    ;;
    type combi = t list
    ;;
    type main = t MultiEnsemble.mset
    ;; 
    type etat = { noms: string array; scores: int array; mains: main array;
		table: combi list; pioche: main; pose: bool array; tour: int}
    ;;

    let (combi_valide : combi -> bool ) = fun c -> (List.length c > 3)
    ;;
      

  end 
;;
