(* GRADE:  100% *)

let choose l =
  List.nth l (Random.int (List.length l))

let choose_elements l n =
  let rec choose_and_remove l res k=
    match l,k with
    | [], x -> failwith "out of bounds"
    | x::xs , 0 -> List.append res xs, x
    | x::xs, m -> choose_and_remove xs ( List.rev (x::(List.rev res))) (m-1)
  and aux l res k =
    match l,k with
    | [] , x -> []
    | x::xs , 0 -> res
    | _ , m -> 
        let t,x = choose_and_remove l [] (Random.int (List.length l)) 
        in (aux t (x::res) (m-1))
  in aux l [] n

let choose_sublist l n =
  let rec choose_and_remove l res k=
    match l,k with
    | [], x -> failwith "out of bounds"
    | x::xs , 0 -> List.append res xs, x
    | x::xs, m -> choose_and_remove xs ( List.rev (x::(List.rev res))) (m-1)
  and aux l res k =
    match l,k with
    | [] , x -> []
    | x::xs , 0 -> l
    | _ , m -> 
        let t,x = choose_and_remove l [] (Random.int (List.length l)) 
        in (aux t (x::res) (m-1))
  in aux l [] ((List.length l) - n)

