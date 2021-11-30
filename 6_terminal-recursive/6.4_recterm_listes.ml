(* GRADE:  100% *)
let insert x l =
  let rec aux res x added = function
    | [] -> if added then res else x::res
    | e::es -> if x<e then
          if added then aux (e::res) x true es 
          else aux (e::x::res) x true es 
        else if e=x then aux (e::res) x true es
        else aux (e::res) x false es in List.rev (aux [] x false l)

let sort l =
  let rec aux res = function
    | [] -> res
    | x::xs -> aux (insert x res) xs in aux [] l

let union_sorted l1 l2 =
  let rec aux res l1 l2 = match l1,l2 with
    | [], l -> List.rev_append res l
    | l, [] ->  List.rev_append res l
    | x::xs, y::ys -> if x=y then aux (x::res) xs ys else 
        if x<y then aux (x::res) xs (y::ys) else aux (y::res) (x::xs) ys 
  in aux [] l1 l2

let inter_sorted l1 l2 =
  let rec aux res l1 l2 = match l1,l2 with
    | x::xs, y::ys -> if x=y then aux (x::res) xs ys else 
        if x<y then aux res xs (y::ys) else aux res (x::xs) ys 
    | _ -> List.rev res in aux [] l1 l2

