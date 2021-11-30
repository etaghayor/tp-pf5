(* GRADE:  61% *)

let iter f vec =
  let rec aux f vec k =
    if k >= vec.size then () else (f vec.contents.(k) k; aux f vec (k+1))
  in aux f vec 0
    
let iter f vec =
  let rec aux f vec k =
    if k >= vec.size then () else 
      try f vec.contents.(k) k with
      | Break -> ()
      | _ -> aux f vec (k+1) 
  in aux f vec 0
