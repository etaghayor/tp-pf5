(* GRADE:  100% *)
let rec desc_neg = function
  | Prop x  -> Prop x
  | Neg (Neg x) -> desc_neg x
  | Neg (Prop x) -> Neg (Prop x)
  | Neg (And (x,y)) -> Or (desc_neg (Neg x),desc_neg (Neg y))
  | Neg (Or (x,y)) -> And (desc_neg (Neg x),desc_neg (Neg y))
  | And (x,y) -> And (desc_neg x, desc_neg y)
  | Or (x,y) -> Or (desc_neg x, desc_neg y)

let rec desc_or = function
  | Or (x,And (y,z)) ->
      And (desc_or (Or (x,y)) ,desc_or (Or (x,z)))
  | Or (And (y,z), x) -> 
      And (desc_or (Or (y,x)) ,desc_or (Or (z,x))) 
  | Or (x,y) -> let dx = desc_or x in 
      ( match dx with 
        | And (a,b) -> desc_or(Or (dx, desc_or y))
        | _ -> let dy = desc_or y in 
            match dy with 
            | And (a,b) -> desc_or(Or (desc_or x, dy))
            | _ -> Or (dx,dy))
  | And (x,y) -> And (desc_or x, desc_or y)
  | x->x
    
let cnf f =
  desc_or (desc_neg f )
  

