(* GRADE:  100% *)
let rec sum n =
  match n with 
  | 0 -> 0
  | _ -> n + sum (n-1)

