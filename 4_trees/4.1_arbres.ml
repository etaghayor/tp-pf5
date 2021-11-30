(* GRADE:  100% *)
let rec size a = 
  match a with
  | Nil -> 0
  | Node (x,y,z) -> size y + size z + 1

let rec depth a =
  match a with
  | Nil -> 0
  | Node (x,y,z) ->  1 + max (depth y) (depth z)

let rec sum a =
  match a with
  | Nil -> 0
  | Node (x,y,z) -> x + sum y + sum z

let rec contains x a =
  match a with
  | Nil -> false
  | Node (e,y,z) -> if x == e then true else (contains x y || contains x z)

let rec elements a =
  (*match a with
   | Nil -> []
   | Node (x,y,z) -> elements y @ [x] @ elements z *)
                      
  let rec aux a res =
    match a with
    | Nil -> res
    | Node (x, Nil, Nil) -> x::res
    | Node (x,y,z) -> ( aux y (x::(aux z res)))
  in aux a []

let rec perfect a =
  if size a == int_of_float (2. ** float_of_int (depth a) -. 1.) then true else false 

