(* GRADE:  100% *)
let is_consequence f g = 
  let inter = interpretations_props (union_sorted (list_of_props f) (list_of_props g)) in
  let rec aux = function
    | [] -> true
    | x::xs -> if  eval_formula f x && not (eval_formula g x )then false
        else aux xs
  in aux inter
    

let are_equivalent f g = is_consequence f g && is_consequence g f

