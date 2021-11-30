(* GRADE:  100% *)
let rec insert x l =
  match l with
  | [] -> [x]
  | e::es -> if x < e then x::l else
      if x = e then l else e::(insert x es)

let rec sort l =
  let rec aux l res =
    match l with 
    | [] -> res
    | x::xs -> aux xs (insert x res)
  in aux l []

let rec mem_sorted x = function 
  | [] -> false
  | e::es -> if e > x then false else x==e || mem_sorted x es
    
let rec union_sorted l1 l2 =
  match l1 with 
  | [] -> l2
  | x::xs -> union_sorted xs (insert x l2)

let rec inter_sorted l1 l2 =
  let rec aux l1 l2 res =
    match l1 with
    | [] -> res
    | x::xs -> if mem_sorted x l2 then aux xs l2 (res @ [x]) else aux xs l2 res
  in aux l1 l2 []
    
let rec quicksort l = match l with 
  | [] -> [] 
  | x :: xs -> 
      let l,r = List.partition (fun a -> a<x) xs in 
      quicksort l @ [x] @ quicksort r 

