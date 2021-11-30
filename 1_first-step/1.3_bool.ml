(* GRADE:  100% *)
let is_zero x =
  if x=0 then true else false

let msg_zero x =
  if x=0 then "zero" else "not zero"

let my_max a b =
  if a>b then a else b

let max_triple a b c =
  max (max a b) c

let max_quadruple a b c d =
  max (max_triple a b c) d

