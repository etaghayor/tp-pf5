(* GRADE:  100% *)
let bis s =
  s^s;;

let times8 s =
  let rec aux str n =
    if n < 2 then str 
    else (aux (bis str) (n/2)) in
  aux s 8;;

let times8_bis s =
  bis (bis (bis s))

