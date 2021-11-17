
type ('a,'b) table_hachage = { hache: 'a -> int; donnees: ('a * 'b) list array; largeur: int };; 

(**[creer h w] create a table_hachage with h the function of hachage, a voided Arrayw of w element and a largeur of w*)
let creer h w = {hache= h ;donnees = Array.make w []; largeur = w};;

(**[recherche t k] tell if there is a key k in t*)
let recherche t k = 
    List.exists(fun c -> fst c = k) t.donnees.(t.hache k)
(**[element t k] take an Array k of elements and return l an Array of every element associated by t, if there is no element associated to k.(i), l.(i) take the value of []*)
let elements t k =
    let l = Array.make(Array.length k) [] in
    for i = 0 to Array.length k - 1 do
        if recherche t k.(i) then l.(i) <- t.donnees.(t.hache k.(i))
        else l.(i) <- []
    done;
    l;;
(**[ajout t k e] add the element e with the key k in t*)
let ajout t k e =
    t.donnees.(t.hache k) <- (k,e)::t.donnees.(t.hache k)










