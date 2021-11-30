(* GRADE:  97% *)
let push_back vec x =
  if Array.length vec.contents <= vec.size then raise (Failure "push_back") else
    vec.contents.(vec.size) <- x; vec.size <- vec.size+1
                                               
let pop_back vec =
  if vec.contents.(0) = vec.default then None 
  else let res = vec.contents.(vec.size-1) in
    vec.contents.(vec.size - 1) <- vec.default; vec.size <- vec.size - 1; Some res;;

let append vec1 vec2 = 
  Array.blit vec2.contents 0 vec1.contents vec1.size vec2.size;
  vec1.size <- vec1.size + vec2.size;;

let resize vec new_size = 
  if new_size < vec.size then raise (Invalid_argument "resize") else
    let tmp = Array.make new_size vec.default in
    Array.blit vec.contents 0 tmp 0 vec.size;
    vec.contents <- tmp;;

let push_back vec x =
  if Array.length vec.contents <= vec.size then resize vec (vec.size*2) else ();
  vec.contents.( vec.size) <- x; vec.size <- vec.size+1
                                             
let pop_back vec (*(with resize)*)=
  let len = Array.length vec.contents in
  if  len >= 4 * vec.size then resize vec (len/2) else ();
  if vec.contents.(0) = vec.default then None else
    let res = vec.contents.(vec.size-1) in
    vec.contents.(vec.size - 1) <- vec.default; vec.size <- vec.size - 1; 
    Some res;;

let append vec1 vec2 = 
  let len = Array.length vec1.contents in
  if  len < vec1.size + vec2.size then resize vec1 (vec1.size+vec2.size) else ();
  Array.blit vec2.contents 0 vec1.contents vec1.size vec2.size;
  vec1.size <- vec1.size + vec2.size;;
  
  
