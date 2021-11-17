type ('a,'b) table_hachage = { hache: 'a -> int; donnees: ('a * 'b) list array; largeur: int };; 

let creer h w = {hache= h ;donnees = Array.make w []; largeur = w};;

let recherche t k = 
    List.exists(fun c -> fst c = k) t.donnees.(t.hache k)

let elements t k =
    let l = Array.make(Array.length k) [] in
    for i = 0 to Array.length k - 1 do
        if recherche t k.(i) then l.(i) <- t.donnees.(t.hache k.(i))
        else l.(i) <- []
    done;
    l;;

let ajout t k e =
    t.donnees.(t.hache k) <- (k,e)::t.donnees.(t.hache k)










