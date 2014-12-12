
module type Multiensemble = 
sig
   type 'a mset
   val vide : 'a mset
   val is_vide : 'a mset -> bool 
   (*val union : 'a mset -> 'a mset -> 'a mset*)
   val appartient : 'a -> 'a mset -> bool
  (* val egal : 'a mset -> 'a mset -> bool*)
end 
;;

module MultiEnsemble: Multiensemble
