(* GRADE:  100% *)

let matrix_map (f: 'a -> 'b) (m: 'a Matrix.t): 'b Matrix.t =
  Matrix.init (Matrix.size m) (fun case -> f (Matrix.get m case))
;;


let gagnant_dim_2 (plateau : plateau): int option =
  gagnant_dim_1 (matrix_map gagnant_dim_1 plateau)
  
;;


let termine_dim_2 (plateau : plateau) : bool =
  match gagnant_dim_2 plateau with
  | Some k -> true
  | None -> termine_dim_1 (matrix_map gagnant_dim_1 plateau)
;;

