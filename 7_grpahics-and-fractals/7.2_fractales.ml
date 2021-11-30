(* GRADE:  100% *)

let rec sierpinski size n =
  if n = 0 then triangle size else 
    [Repeat (3, [Repeat(1,(sierpinski (size /. 2.) (n-1)));Repeat(1,[(Move size);(Turn 120.)])])]

let rec kochline size n = 
  if n = 0 then [Line size] else
    let k = Repeat (1,(kochline (size /. 3.) (n-1))) in
    [k;(Turn (-60.));
     k;(Turn (120.)); k; (Turn (-60.)); k]
  
let koch size n = [Repeat (3, [Repeat (1,kochline size n); (Turn 120.)])]

let rec rev_opp l = let rec aux res = function
    | [] -> res
    | x::xs -> match x with 
      | Turn a -> aux ((Turn (-.a))::res) xs
      | Repeat (r,c) -> aux((Repeat (r, (rev_opp c)))::res) xs
      | _ -> aux (x::res) xs 
  in aux [] l

let rec dragon size n = 
  if n = 0 then [Line size] else
    let size2 = size /. sqrt 2. in
    [Repeat (1, (dragon size2 (n-1))); (Turn 90.);
     Repeat (1,(rev_opp (dragon size2 (n-1))))]

let interleave x f l =
  let rec aux res x f = function
    | [] -> List.rev (x::res)
    | hd::tl -> aux (hd::x::res) (f x) f tl in
  aux [] x f l

let rec dragon_angles n = 
  if n = 0 then [] else
    interleave  90. (fun x -> (-.x)) (dragon_angles (n-1))

let dragon2 size n = 
  let size2 =  size /. (Float.pow (sqrt 2.) (float_of_int n)) in
  interleave (Line size2) (fun x -> x) (List.map (fun x -> (Turn x)) (dragon_angles n))
  
  

