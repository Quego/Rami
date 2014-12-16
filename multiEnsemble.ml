(*

module type MultiEnsemble = 
sig
   type 'a mset
   val vide : 'a mset
   val is_vide : 'a mset -> bool 
   (*val union : 'a mset -> 'a mset -> 'a mset*)
   val appartient : 'a -> 'a mset -> bool
  (* val egal : 'a mset -> 'a mset -> bool*)
end 
;;
*)   
Random.self_init; 

module MultiEnsemble =
struct

  type 'a mset = ('a*int) list
  ;;

  let (vide: ('a * int) list ) = []
  ;;
  
  let (is_vide : ('a *int) list -> bool) = fun m ->
match m with
  |[] -> true
  | _ -> false
    ;;

  let rec (add: 'a -> ('a * int) list -> ('a * int ) list) = fun x l->
    match l with
    |[] -> [(x,1)]
    |(w,n)::ls -> 
       if w = x then
	 (w,n+1)::ls
       else
	 (w,n)::(add x ls)

  let rec (supp : ('a*int) -> ('a * int ) list -> ('a * int ) list ) = fun (x,i) l1 ->
    match l1 with
      |[] -> []
      |(w,n)::ls -> if (x = w && n > i)
	then (x,n-i)::ls
	else if (x = w )
	then ls
	else (w,n)::(supp (x,i) ls)
  ;;

    let rec (union : ('a * int ) list -> ('a * int ) list -> ('a * int ) list ) = fun l1 l2 ->
match l1 with 
  |[] -> l2
  |(x,n)::q -> let new_n= n + (try List.assoc x l2 with |Not_found -> 0) in
       (x,new_n)::(union (List.remove_assoc x l2) q)  
    ;;

    let rec (appartient : 'a -> ('a *int ) list -> bool ) = fun x m ->
      match m with
	|[] -> false
	| (t,_)::q -> if (x==t) then true
	  else appartient x q
    ;;
    
  let rec (egal : ('a * int) list -> ('a * int) list -> bool ) = fun l1 l2 ->
    match l2 with
    |[] -> (l1==[])
    |(x,n)::ls -> List.mem (x,n) l1 && egal (List.remove_assoc x l1) ls
    ;;


  let rec (add : 'a ->  ('a *int) list -> ('a * int ) list) = fun x xl ->
    match xl with
      |[] -> [(x,1)]
      |(t,n)::l -> if x = t then
	  (t,n+1)::l
	else
	  (t,n)::(add x l)
  ;;


  let rec (taille : ('a * int ) list -> int ) = fun l ->
    match l with
    |[] -> 0
    |(x,n)::l -> n + taille l
  ;;

(* FONCTION A REVOIR *)
  let (rand : ('a * int) list -> 'a * ('a * int ) list ) = fun x ->
    let tx = taille x 
    in let u = Random.int tx 
       in let rec (rand_aux : int -> ('a * int) list -> 'a *('a *int) list) = fun i l->
	 match l with
	   |[] -> failwith "Cas a traiter"
	   |t::q -> 
	       let (a,b) = t
	       in if (i < b) then 
		   if (b=1) then
		     (a,q)
		   else
		     (a,(a,b-1)::q) 
		 else
		   let (r,new_l)= rand_aux (i-b) q
		   in (r,t::new_l)
	  in rand_aux u x
  ;;

end

