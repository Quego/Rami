#use "multiensemble.ml"

module Lettres =
  struct 
    type t
    type combi = t list
    type main = t MultiEnsemble.mset 
    type etat = { noms: string array; scores: int array; mains: main array;
		table: combi list; pioche: main; pose: bool array; tour: int}

  end
;;
