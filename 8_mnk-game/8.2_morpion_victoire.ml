(* GRADE:  100% *)
let ligne (m : 'a Matrix.t) (i : int) : 'a list =
  List.init (Matrix.size m) (fun x -> Matrix.get m (i,x))
;;

let colonne (m : 'a Matrix.t) (j : int) : 'a list = 
  List.init (Matrix.size m) (fun x -> Matrix.get m (x,j))
;;

let diagonale (m : 'a Matrix.t) (k : int) : 'a list = 
  let s = Matrix.size m in
  List.init s (fun x -> Matrix.get m (x , if k=0 then x else s - x - 1))
;;

let gagnant_liste (l : 'a option list) : 'a option =
  if l = [] then None else
  if List.for_all (fun x -> x = (List.hd l)) l then List.hd l else None
;;

let premier_succes (n : int)(f : int -> 'a option) : 'a option =
  let l = List.init n f  in
  match List.find (fun x -> x != None) l with 
  | x -> x
  | exception _ -> None
;;

let rec aux_winner m i t = 
  if i = Matrix.size m then None else
    let lcd = match t with
      | 0 -> ligne m i
      | 1 -> colonne m i
      | _ -> diagonale m i in 
    (match gagnant_liste lcd with
     | None -> aux_winner m (i+1) t
     | t -> t) ;;

let gagnant_lignes (m : 'a option Matrix.t) : 'a option =
  aux_winner m 0 0
;;

let gagnant_colonnes (m : 'a option Matrix.t) : 'a option =
  aux_winner m 0 1
;;

let gagnant_diagonales (m : 'a option Matrix.t) : 'a option =
  aux_winner m 0 2
;;

let gagnant (m : 'a option Matrix.t) : 'a option =
  match gagnant_lignes m with
  | Some k -> Some k
  | None -> (match gagnant_colonnes m with
      | Some k -> Some k
      | none -> (match gagnant_diagonales m with
          | Some k -> Some k
          | None -> None
        ))
    
let termine (m : 'a option Matrix.t) : bool =
  match gagnant m with
  | Some k -> true 
  | None -> let rec aux m = function
      | 0 -> true
      | k -> match List.find (fun x -> x = None) (ligne m k) with
        | x -> false
        | exception _ -> aux m (k-1) 
      in aux m (Matrix.size m - 1) 
;;

