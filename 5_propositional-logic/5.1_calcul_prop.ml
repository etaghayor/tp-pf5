(* GRADE:  100% *)
let rec string_of_formula f =
  match f with
  | Prop x -> x 
  | Neg x -> "Neg " ^ (string_of_formula x)
  | And (x,y) -> "(" ^ (string_of_formula x) ^ " And " ^ (string_of_formula y) ^")"
  | Or (x,y) -> "(" ^ (string_of_formula x) ^ " Or " ^ (string_of_formula y) ^")"

let rec list_of_props f =
  let rec aux res = function
    | Prop x -> x::res 
    | Neg x -> aux res x
    | And (x,y) | Or (x,y) -> union_sorted (aux res x) (aux res y) 
  in aux [] f

let rec eval_formula f l =
  match f with
  | Prop x -> List.assoc x l
  | Neg x -> not (eval_formula x l)
  | And (x,y) -> eval_formula x l && (eval_formula y l)
  | Or (x,y) ->  eval_formula x l || (eval_formula y l)

