(* GRADE:  100% *)





let rec string_of_expr = function
  | Const f -> string_of_float f 
  | Var var -> var 
  | Uop (op,e) -> string_of_uop op ^ "(" ^ string_of_expr e ^ ")"
  | Bop (op,e1,e2) -> 
      "(" ^ string_of_expr e1 ^ " " ^ string_of_bop op ^ " " ^ string_of_expr e2 ^ ")"
  | App (f,e) -> "(" ^ string_of_fonction f ^ ")(" ^ string_of_expr e ^ ")"
    
and string_of_fonction f =
  f.param ^ " |-> " ^ string_of_expr f.corps 

let rec eval_expr env = function
  | Const f -> f
  | Var var -> List.assoc var env
  | Uop (op,e) -> eval_uop op (eval_expr env e)
  | Bop (op,e1,e2) -> eval_bop op (eval_expr env e1) (eval_expr env e2)
  | App (f,e) -> eval_fonction env f (eval_expr env e)
    
and eval_fonction env f arg =
  eval_expr ((f.param,arg)::env) f.corps

let simpl_plus e1 e2 =
  match e1,e2 with
  | Const f1, Const f2 -> Const (f1 +. f2)
  | Const 0. , t -> t
  | t, Const 0. -> t
  | _ -> Bop (Plus,e1,e2)

let simpl_moins e1 e2 =
  match e1,e2 with
  | Const f1, Const f2 -> Const (f1 -. f2)
  | Const 0. , t -> t
  | t, Const 0. -> t
  | _ -> Bop (Moins,e1,e2)

let simpl_fois e1 e2 =
  match e1,e2 with
  | Const f1, Const f2 -> Const (f1 *. f2)
  | Const 1. , t -> t
  | t, Const 1. -> t
  | _ -> Bop (Fois,e1,e2)

let simpl_bop op e1 e2 =
  match op with
  | Plus -> simpl_plus e1 e2
  | Moins -> simpl_moins e1 e2
  | Fois -> simpl_fois e1 e2

let simpl_uop op e =
  match e with
  | Const f -> Const (eval_uop op f)
  | Bop (b,e1,e2) -> Uop (op, (match simpl_bop b e1 e2 with
      | Const f -> Const (eval_uop op f)
      | e -> e))
  | t -> Uop (op,t)

let rec simpl_expr = function
  | Uop (op, e) -> simpl_uop op (simpl_expr e)
  | Bop (b,e1,e2) -> simpl_bop b (simpl_expr e1) (simpl_expr e2)
  | App (f,e) -> App (simpl_fonction f, simpl_expr e)
  | t -> t

and simpl_fonction f =
  {param = f.param ; corps=simpl_expr f.corps}

