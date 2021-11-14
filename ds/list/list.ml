(** [sum l] returns the sum of elements of [l] *)
let rec sum l = match l with
    | [] -> 0
    | e::q -> e + sum q;;
    
(**[lenght l] return the length of [l]*)
let rec length l = match l with
    |[] -> 0
    |e::q -> 1 + length q;;
(**[map f l] return f(e) with e element of [l]*)    
let rec map f l = match l with
    |[] -> []
    |e::q -> f e:: map f q;;
(**[filtre f l] return the element of [l] which verify f*)
let rec filtre f l = match l with
    |[] -> []
    |e::q -> if f e then e::filtre f q
                    else filtre f q;;
                    
                  
 (**[ajout x l] add x at [l] *)                              
let ajout x l = match l with
    |[] -> [x]
    |e::q -> x::e::q;;
(**[present x l] verify if x is in [l] *)     
    let rec present x l = match l with
    |[] -> false
    | e::q ->  x = e || present x q;;
