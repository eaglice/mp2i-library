type 'a zipper = {left : 'a list; right: 'a list };;

let rec move_right z x = match z.right with
    |[]-> failwith "rien à droite"
    |e::q -> if x = 0 then z 
            else move_right {left = e::z.left; right= q} (x-1);;
    

let add a z = {left = z.left ; right = a::z.right };;

let rec del z x = match z.right with
    |[] -> {left = z.left ; right = z.right}
    |e::q -> if x = 0 then z
            else del {left = z.left ; right = q} (x-1);;