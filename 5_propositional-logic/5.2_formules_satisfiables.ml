(* GRADE:  100% *)
let add_to_all x ll =
  List.map (fun y -> x::y) ll;;

let rec interpretations_props  = function
  | [] -> [[]]
  | x::xs -> add_to_all (x, false) (interpretations_props xs) @
             (add_to_all (x, true) (interpretations_props xs) )
                                                             
let interpretations f =
  interpretations_props (list_of_props f)

let satisfiable f =
  let rec aux = function
    | [] -> false
    | x::xs -> if eval_formula f x then true else aux xs
  in aux (interpretations f)

let tautology f =
  let rec aux = function
    | [] -> true
    | x::xs -> if eval_formula f x = false then false else aux xs
  in aux (interpretations f)

