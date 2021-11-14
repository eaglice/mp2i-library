(** [recherche_dicho t x] tell if x is in [t] by using dichotomie without using a recursive function *)
let recherche_dicho t x = 
    let e = Array.length t -1 in 
    let (a,b) = ( 0,  e) in
    let (d,f) = (ref a, ref b) in
            while (!d < !f ) do
                let m = (!d + !f)/2 in 
                if t.(m) < x then
                    d := m + 1
                else
                    f := m; 
            done;
            t.(!d) = x;;
(** [dicho_rec t x] tell if x is in [t] by using dichotomie with  a recursive function *)
let dicho_rec t x =
    let rec aux i j =
    if i > j then false
    else let m = (i + j)/2 in 
        if t.(m) = x then true
        else if t.(m) < x then aux (m + 1) j
        else aux i (m- 1)
    in aux 0 (Array.length t - 1)

(** [tricho_rec t x] tell if x is in [t] by using trichotomie with  a recursive function *)
let tricho_rec t x =
    let rec aux i j =
        if i > j then false
        else let m1 = (2*i + j + 1 )/ 3 in
            let m2 = ( i + 2*j + 2)/3 in
            if t.(m1) = x || t.(m2) = x then true
            else if x < t.(m1) then aux i (m1-1)
            else if x < t.(m2) then aux (m1 + 1) (m2 - 1)
            else aux (m2 + 1 ) j in
    aux 0 (Array.length t -1)


