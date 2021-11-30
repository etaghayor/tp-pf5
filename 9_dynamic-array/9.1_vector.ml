(* GRADE:  100% *)
let create s def = {contents = Array.make s def; default = def; size = 0}
                   
let of_list l def cap = 
  let res = create cap def in
  let rec aux res i = function
    | [] -> res
    | x::xs -> (res.contents.(i) <- x; res.size <- res.size + 1; aux res (i+1) xs)
  in aux res 0 l
    
let get vec pos =
  if pos < vec.size then vec.contents.(pos) else raise (Invalid_argument "get" )
      
let set vec pos x =
  if pos < vec.size then vec.contents.(pos) <- x else raise (Invalid_argument "set" )
      
      (* TODO *)
let equal eq vec1 vec2 = 
  let rec aux k =
    if k = vec1.size && k = vec2.size then true else 
    if eq vec1.contents.(k) vec2.contents.(k) then aux (k+1) else false
  in aux 0
    
let clear vec = 
  Array.fill vec.contents 0 vec.size vec.default; vec.size <- 0;;
    
