(* GRADE:  100% *)
let rec size1 =function
  | Nil -> 0
  | Node (_,y,z) -> 1+ size1 y + size1 z

let size' l n =
  let rec aux n = function
    | [] -> n
    | x::xs -> match x with
      | Nil -> aux n xs
      | Node (_,t1,t2) -> aux (n+1) (t1::t2::xs) 
  in aux n l

let size a = size' [a] 0

