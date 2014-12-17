   
Random.self_init; 

module MultiEnsemble =
struct

  type 'a mset = ('a*int) list
  ;;


(* ============= VIDE ============*)
(* Crée un multisensemble vide *)

  let (vide: ('a * int) list ) = []
  ;;


(* ============== IS EMPTY ========== *)
  
(* Renvoie true si un mutliensemble est vide 
Prends un multiensemble en entrée renvoie un boolean *)
  let (is_vide : ('a *int) list -> bool) = fun m ->
match m with
  |[] -> true
  | _ -> false
    ;;


 (* ======================== ADD ==================== *)

(* Ajout un élément à un mutliensemble 
Prends un éléments et un mutliensemble ,renvoie le multiensemble avec l'éléments en plus *)

  let rec (add: 'a -> ('a * int) list -> ('a * int ) list) = fun x l->
    match l with
    |[] -> [(x,1)]
    |(w,n)::ls -> 
       if w = x then
	 (w,n+1)::ls
       else
	 (w,n)::(add x ls)

 (* ======================== SUPP ====================== *)

(* Supprime un élément à un mutliensemble 
Prends un éléments et un mutliensemble ,renvoie le multiensemble avec l'éléments en moins*)


  let rec (supp : ('a*int) -> ('a * int ) list -> ('a * int ) list ) = fun (x,i) l1 ->
    match l1 with
      |[] -> []
      |(w,n)::ls -> if (x = w && n > i)
	then (x,n-i)::ls
	else if (x = w )
	then ls
	else (w,n)::(supp (x,i) ls)

  ;;

(* ============ UNION ================== *)

(* Fait l'union de deux multiensemble 
Prends deux multiensemble et renvoie un mutliensemble *)


    let rec (union : ('a * int ) list -> ('a * int ) list -> ('a * int ) list ) = fun l1 l2 ->
match l1 with 
  |[] -> l2
  |(x,n)::q -> let new_n= n + (try List.assoc x l2 with |Not_found -> 0) in
       (x,new_n)::(union (List.remove_assoc x l2) q)  
    ;;

(* ========= APPARTIENT =============*)

(* Prends un éléments et un mutliensemble ,renvoie true si l'élement est dans le mutliensemble*)


    let rec (appartient : 'a -> ('a *int ) list -> bool ) = fun x m ->
      match m with
	|[] -> false
	| (t,_)::q -> if (x==t) then true
	  else appartient x q
    ;;
 (* Prends deux multiensemble ,renvoie true si ils sont égaux *)   

  let rec (egal : ('a * int) list -> ('a * int) list -> bool ) = fun l1 l2 ->
    match l2 with
    |[] -> (l1==[])
    |(x,n)::ls -> List.mem (x,n) l1 && egal (List.remove_assoc x l1) ls
    ;;

  (* Calcul la taille d'un multisemble *) 

  let rec (taille : ('a * int ) list -> int ) = fun l ->
    match l with
    |[] -> 0
    |(x,n)::l -> n + taille l
  ;;

(* FONCTION A REVOIR *)
(* ========== RAND =========== *)
(* Prends un mutliensemble et renvoie un couple d'un element aleatoire du multiensemble et le multiensemble moins cet element *)


  let (rand : ('a * int) list -> 'a * ('a * int ) list ) = fun x ->
    let tx = List.length x
    in let u = (Random.int tx) 
       in let rec (rand_aux : int -> ('a * int) list -> 'a *('a *int) list) = fun i l->
	 if (i>0) 
	 then match l with
		|[] -> failwith "Unreacheable case ( RAND )"
		|(t::q) -> let(r,l) = rand_aux (i-1) q in (r,t::l)
	 else
	   match l with
	     |[] -> failwith "Unreacheable case (RAND )"
	     |t::q -> 
	       let (a,b) = t
	       in  if (b=1) then
		   (a,q)
		 else
		   (a,(a,b-1)::q) 
	  in rand_aux u x
  ;;

end

