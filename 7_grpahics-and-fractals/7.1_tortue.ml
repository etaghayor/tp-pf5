(* GRADE:  100% *)

let make_turtle x y = {x = x; y=y; angle=0.;path = (Path.moveto x y Path.empty)}
                      
let make_turtle_complet var_x var_y var_angle var_path = 
  {x = var_x; y=var_y; angle=var_angle; path = var_path}
  
let forward dist trace t = 
  let y_dist = sin ((t.angle *. Float.pi) /. 180.) *. dist in
  let x_dist = cos ((t.angle *. Float.pi) /. 180.) *. dist in
  let path2 = if trace then Path.lineto (x_dist +. t.x) (y_dist +. t.y) t.path else
      Path.moveto (x_dist +. t.x) (y_dist +. t.y) t.path in 
  make_turtle_complet (x_dist +. t.x) (y_dist +. t.y) t.angle path2


let rec run cmds t = match cmds with
  | [] -> t
  | x::xs -> match x with
    | Line l -> run xs (forward l true t)
    | Move m -> run xs (forward m false t)
    | Turn turn -> run xs (make_turtle_complet t.x t.y (t.angle +. turn) t.path)
    | Repeat (r,c) -> let rec aux = function
        | 0 -> run xs t
        | k -> run (Repeat(r-1,c)::xs) (run c t) in aux r
          
let triangle size = [Repeat (3, [Line size; Turn 120.])]

let square size = [Repeat (4, [Line size; Turn 90.])]

let polygon n size = [Repeat (n, [Line size; Turn (360. /. float_of_int n)])]

                     
let spiral size factor angle n = 
  let l = List.init (2*n) (fun i-> Line size) in
  List.mapi (fun i -> fun x -> if i mod 2 = 0 then
                (Line (size *. (Float.pow factor (float_of_int (i/2)))))
              else (Turn angle)
            ) l 
       

