(* GRADE:  100% *)
let plateau_initial (taille : int) : plateau =
  Matrix.init taille (fun (x,y) -> None)
;;

let coup_legal (plateau : plateau) (coup : coup) : bool =
  Matrix.get plateau coup.position = None
;;

let prepare_coup
    (plateau : plateau)
    (dernier_coup : coup option)
    (nb_joueurs : int)
    (x , y : int*int)
  : coup option =
  let size = Matrix.size plateau in
  if x < 0 || y < 0 || x >= size || y >= size then None else
  
    let turn = match dernier_coup with 
      | None -> 0 | Some c -> ((c.joueur + 1) mod nb_joueurs) in
    Some {joueur = turn ; position = (x,y)}
;;

