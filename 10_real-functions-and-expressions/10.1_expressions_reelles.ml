(* GRADE:  100% *)
let string_of_uop = function
  | Cos -> "cos"
  | Sin -> "sin"
    
let string_of_bop = function
  | Plus -> "+"
  | Moins -> "-"
  | Fois -> "*"

let rec string_of_expr = function
  | Const f -> string_of_float f
  | Var s -> s 
  | Uop (op,e) -> string_of_uop op ^ "(" ^ string_of_expr e ^ ")"
  | Bop (op,e1,e2) -> 
      "(" ^ string_of_expr e1 ^ " " ^ string_of_bop op ^ " " ^ string_of_expr e2 ^ ")"

let rec eval_uop = function
  | Cos -> cos 
  | Sin -> sin 

let eval_bop = function
  | Plus -> (+.)
  | Moins -> (-.)
  | Fois -> ( *. )

let rec eval_expr env  = function
  | Const f -> f
  | Var var -> List.assoc var env
  | Uop (op,e) -> eval_uop op (eval_expr env e)
  | Bop (op,e1,e2) -> eval_bop op (eval_expr env e1) (eval_expr env e2)

