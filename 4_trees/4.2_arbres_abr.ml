(* GRADE:  100% *)
let rec contains_bst x a =
  match a with
  | Nil -> false
  | Node (t,y,z) -> if x==t then true else 
      if x < t then contains_bst x y else contains_bst x z 

let rec add_bst x a =
  match a with
  | Nil -> Node (x,Nil,Nil)
  | Node (e,y,z) -> if x == e then a else 
      if x < e then Node (e, add_bst x y, z) else Node (e, y, add_bst x z)

let rec bst_of_list l =
  match l with 
  | [] -> Nil
  | x::xs -> add_bst x (bst_of_list xs)

let rec bst_of_list_opt l =
  let left,right = List.partition (fun x -> x < (List.nth l ((List.length l)/2))) l in
  match left,right with
  | [],y -> bst_of_list y
  | x, [] -> bst_of_list x
  | x::xs,y::ys -> Node (y , bst_of_list_opt (x::xs), bst_of_list_opt ys)
                 
                     
let rec elements a =    
  let rec aux a res =
    match a with
    | Nil -> res
    | Node (x, Nil, Nil) -> x::res
    | Node (x,y,z) -> (aux y (x::(aux z res)))
  in aux a [] 
    
let rec is_sorted = function
  | [] -> true
  | [x] -> true
  | x::y::xs -> if x>=y then false else is_sorted (y::xs)
                      
let rec is_bst a = is_sorted (elements a)
