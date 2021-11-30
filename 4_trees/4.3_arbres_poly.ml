(* GRADE:  100% *)
let rec forall_labels p a =
  match a with
  | Nil -> true
  | Node (x,y,z) -> p x && forall_labels p y && forall_labels p z

let is_uniform x a =
  forall_labels (fun y -> x==y) a

let rec forall_subtrees pn a =
  match a with
  | Nil -> true
  | Node (x,y,z) -> pn x y z && forall_subtrees pn y && forall_subtrees pn z 

let is_right_comb a =
  forall_subtrees (fun x y z-> y==Nil) a

let sum a =
  fold_tree (fun x y z -> x+y+z) 0 a 

let map_tree f a =
  fold_tree (fun x y z -> Node (f x, y, z)) Nil a

