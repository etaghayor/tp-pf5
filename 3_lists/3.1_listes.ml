(* GRADE:  100% *)
(* Toutes les fonction sont ici définies par des "let rec". Il est
   possible que dans vos solutions certaines fonctions ne soient pas
   récursives, car elles utilisent une fonction recursive auxiliaire
   définie localement. À vous d'enlever le mot clef "rec" dans ce
   cas. *)

let rec list_length l =
  match l with
  | [] -> 0
  | x::xs -> 1 + list_length xs

let rec list_product l =
  match l with
  | [] -> 1
  | x::xs -> x * list_product xs

let rec mem x l =
  match l with 
  | [] -> false
  | y::ys -> if ( y == x) then true else mem x ys

let rec list_min l =
  match l with 
  | []-> failwith "empty list"
  | x::[] -> x
  | x::xs -> min x (list_min xs)
      
let rec last l =
  match l with 
  | []-> failwith "empty list"
  | x::[] -> x
  | x::xs -> last xs

let rec is_sorted l =
  match l with
  | [] -> true
  | x::[]-> true
  | x::y::xs -> if (x>y) then false else is_sorted (y::xs)

let rec average l =
  let rec aux l sum len =
    match l with 
    | [] -> failwith "empty list"
    | x::[] -> (sum+x) / (len+1)
    | x::xs -> aux xs (sum+x) (len+1)
  in aux l 0 0

let rec nth l k =
  match l,k with
  | [], x -> failwith "out of bounds"
  | x::xs , 0 -> x
  | x::xs, m -> nth xs (m-1)

let rec range n m = 
  let rec aux n m =
    if(n>m) then n::(aux (n-1) m) else
    if (n<m) then n::(aux (n+1) m) else [n]
  in aux n m

