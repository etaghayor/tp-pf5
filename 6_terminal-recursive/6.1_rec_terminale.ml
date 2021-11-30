(* GRADE:  100% *)
let listn1 n =
  let rec aux res k = if k=n then n::res else aux (k::res) (k+1)
  in aux [] 0

let length1 l =
  let rec aux res = function
    | [] -> res
    | x::xs -> aux (res+1) xs
  in aux 0 l

