(* GRADE:  100% *)
let plateau_initial (taille : int) : plateau =
  Matrix.init taille (fun (x,y) -> Matrix.init taille (fun (x,y) -> None))
;;

let sous_morpion_valide
    (plateau : plateau)
    (dernier_coup : coup option)
    ((i,j) : int * int)
  : bool = 
  match dernier_coup with
  | None -> true
  | Some coup -> let lp = coup.position_dim_1 in 
      if termine_dim_1 (Matrix.get plateau lp) then true else lp =(i,j)
;;

let coup_legal
    (plateau : plateau)
    (dernier_coup : coup option)
    (coup : coup)
  : bool = 
  if Matrix.get (Matrix.get plateau (coup.position_dim_2)) coup.position_dim_1 <> None then false else
    sous_morpion_valide plateau dernier_coup coup.position_dim_2
;;

let prepare_coup
    (plateau : plateau)
    (dernier_coup : coup option)
    (nb_joueurs : int)
    ((x,y) : int * int)
  : coup option =
  let size = Matrix.size plateau in
  if x < 0 || y < 0 || x >= size*size || y >= size*size then None else 
    let turn = match dernier_coup with 
      | None ->0 | Some c -> ((c.joueur + 1) mod nb_joueurs) in
    Some {joueur = turn ; position_dim_2 = (x/size,y/size) ;position_dim_1 = (x mod size ,y mod size)}
;;

