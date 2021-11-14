(* This file contains searching algorithms *)

(** [swap t i j] exchanges [t.(i)] and [t.(j)] *)
let swap t i j =
  let tmp = t.(i) in
  t.(i) <- t.(j);
  t.(j) <- tmp;;

(** [sort_bubble t] sorts array t using bubble sorting (O(n**2)) *)
let sort_bubble t = 
  for _ = 0 to Array.length t - 1 do
    for j = 0 to Array.length t - 2 do
      if t.(j) > t.(j + 1) then swap t j (j + 1)
      done
  done;;
(** [split l] split t in two list  *)
let rec split = function
    |[] -> [], []
    |[e] -> [e], []
    |e1::e2::q -> let q1, q2 = split q in
    e1::q1, e2::q2;;
  
 (** [fusion l1 l2] merge two sorted list  *)
 let rec fusion l1 l2 = match l1 ,l2 with
    |[], _ -> l2
    |_, [] -> l1
    |e1::q1 ,e2::q2 when e1 < e2 -> e1::fusion q1 l2
    |e1::q1, e2::q2 -> e2::fusion l1 q2;;

(** [tri l] use split and fusion to sort l *)
let rec tri = function
    |[] -> []
    |[e] -> [e]
    |l -> let l1, l2 = split l in
    fusion (tri l1) (tri l2);;
 (** [partition l p] create two list with each element of l1 <e and each element of l2 >= 2*)   
 let rec partition l p = match l with
    |[] -> [],[]
    |e::q -> let l1 ,l2 = partition q p in
        if e < p then e::l1, l2
        else l1, e::l2;;
  (** [tri_rapide l] use partition to create two sorted list from l with each element of (l1) < each element of (l2) then concatenate then to  sort l *)
  let rec tri_rapide = function
    |[] -> []
    | p::q -> let l1, l2 = partition q p in 
            (tri_rapide l1)@(tri_rapide l2)
