
module type MultiEnsemble = 
sig
   type 'a mset
   val vide : 'a mset -> bool 
   val union : 'a mset -> 'a mset -> 'a mset
   val appartient : 'a -> 'a mset -> bool
   val egal : 'a mset -> 'a mset -> bool
end 
;;
