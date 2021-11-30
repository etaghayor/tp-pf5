(* GRADE:  100% *)
(* Toutes les fonctions sont ici définies par des "let rec". Il est
   possible que dans vos solutions certaines fonctions ne soient pas
   récursives, car elles utilisent une fonction récursive auxiliaire
   définie localement. À vous d'enlever le mot clef "rec" dans ce
   cas. *)

let rec map f l =
  match l with 
  | [] -> []
  | x::xs -> (f x)::(map f xs)

let rec filter p l =
  match l with
  | [] -> []
  | x::xs -> if (p x) then x::(filter p xs) else filter p xs 

let rec append l1 l2 =
  match l1 with
  | [] -> l2
  | x::xs -> x::(append xs l2)
                 
let rec rev l =
  let rec aux l res=
    match l with
    | [] -> res
    | x::xs -> aux xs (x::res)
  in aux l []
           

let rec flatten l =
  match l with 
  | [] -> [] 
  | x::[] -> x
  | x::xs -> append x (flatten xs) 
                                

let rec rotation_d l =
  match rev l with
  | [] -> []
  | x::xs -> x::(rev xs)

