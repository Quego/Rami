module MultiEnsemble =
  struct

    type 'a mset = ('a*int) list
    ;;

    let (vide: 'a mset ) = []
    ;;

    let (is_vide : 'a mset -> bool) = fun m ->
match m with
  |[] -> true
  | _ -> false
    ;;

(* ON NE S'EN SERT PAS 
    let rec (union : 'a mset -> 'a mset -> 'a mset ) = fun m1 m2 ->
match m1 with 
  |[] -> m2
  | (x,i)::q -> match m2 with
      
    ;;*)

    let rec (appartient : 'a -> 'a mset -> bool ) = fun x m ->
match m with
  |[] -> false
  | (t,_)::q -> if (x==t) then true
    else appartient x q
      ;;
     
(*ON NE S'EN SERT PAS  
    let rec (egal : 'a mset -> 'a mset -> bool ) = fun m1 m2 ->
      let n1 = (List.sort Pervasives.compare m1) and n2 = (List.sort Pervasives.compare m2) in (*Tri les ensembles pour les comparer *)
      let rec (meme_list : 'a mset -> 'a mset -> bool) = fun l1 l2 ->
	  match (l1,l2) with
	    | ([],[]) -> true
	    |(x1::r1,x2::r2) -> if x1=x2 then meme_list r1 r2 else false
	    |_ -> false   in 
      meme_list n1 n2
    ;;
*)	  

  end

;;


(*

#trace MultiEnsemble.egal;;
MultiEnsemble.egal [1;4;5] [1;4;5];;
let m1 = [1;4;5];;
let m2 = [1;4;5];;
     let ( comp : 'a -> 'a -> int ) = fun x1 x2 ->
	if (x1 > x2) then 1
	else if ( x1 < x1 ) then -1
	else 0
;;

     let test = (List.sort comp m1 == List.sort comp m1);;
     let test = [1;4;5] == [1;4;5];;
*)
