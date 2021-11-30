(* GRADE:  100% *)
let rev_append left right =
  let rec aux res = function
    | [] -> res
    | x::xs-> aux (x::res) xs
  in aux right left
    
let append left right =
  rev_append (List.rev left) right

