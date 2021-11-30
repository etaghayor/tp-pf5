(* GRADE:  51% *)


let rec deriv_expr x = function
  | Const f -> Const 0.
  | Var y -> if x = y then Const 1. else Const 0.
  | Uop (Cos,e) -> Bop (Moins,
                        Const 0.,
                        Bop (Fois, 
                             deriv_expr x e,
                             Uop (Sin,e)))
  | Uop (Sin,e) ->  Bop (Fois, 
                         deriv_expr x e,
                         Uop (Cos,e))
  | Bop (Fois,e1,e2) -> Bop (Plus,
                             Bop (Fois,
                                  deriv_expr x e1,
                                  e2),
                             Bop (Fois,
                                  e1,
                                  deriv_expr x e2))
  | Bop (op,e1,e2) -> Bop (op, deriv_expr x e1,deriv_expr x e2)
  | App (f,e) -> App (deriv_fonction f, e)

and deriv_fonction f =
  {param = f.param ; corps = deriv_expr f.param f.corps}

let newton_iter f' xi yi =
  "Remplacer cette chaîne par votre code"

let newton f x0 =
  "Remplacer cette chaîne par votre code"

let resout_eq f g xini =
  "Remplacer cette chaîne par votre code"

let extremum_local f x0 =
  "Remplacer cette chaîne par votre code"

