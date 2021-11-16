type 'a zipper = {left : 'a list; right: 'a list };;

(**[move_right z x] move the zipper to the right by x case*)
let rec move_right z x = match z.right with
    |[]-> failwith "rien à droite"
    |e::q -> if x = 0 then z 
            else move_right {left = e::z.left; right= q} (x-1);;
    
(**[add a z] add the element a to the right of the zipper*)
let add a z = {left = z.left ; right = a::z.right };;

(**[del z x] del x element of the right of z*)
let rec del z x = match z.right with
    |[] -> {left = z.left ; right = z.right}
    |e::q -> if x = 0 then z
            else del {left = z.left ; right = q} (x-1);;
