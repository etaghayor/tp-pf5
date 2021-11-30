(* GRADE:  100% *)
let rec fibo n =
  match n with 
  | 0 -> 1
  | 1 -> 1
  | _ -> fibo (n-1) + fibo (n-2)

