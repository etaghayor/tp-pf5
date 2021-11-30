(* GRADE:  100% *)
let rec pasA n =
  match n with 
  | 0 -> 0
  | _ -> 1 + pasB (n-1)

and pasB n =
  match n with 
  | 0 -> 0
  | _ -> if ( n mod 2 == 0 ) then (1 + pasA (n-2)) else (1 + pasA (n-1))

