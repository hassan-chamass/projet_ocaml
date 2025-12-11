(* ----------------------------------------------------------- *)
(* Fichier: vector2D.ml *)
(* Description: Module pour les opérations vectorielles en 2D. *)
(* ----------------------------------------------------------- *)


(* A REECRIRE !!!!!!!!!*)
type t = { x : float; y : float };;

let make x y = { x; y };;
let zero = make 0.0 0.0;;

let add v1 v2 = make (v1.x +. v2.x) (v1.y +. v2.y);;
let sub v1 v2 = make (v1.x -. v2.x) (v1.y -. v2.y);;
let scale s v = make (s *. v.x) (s *. v.y);;
let dot v1 v2 = (v1.x *. v2.x) +. (v1.y *. v2.y);;
let norm_sq v = dot v v;;
let norm v = sqrt (norm_sq v);;
let normalize v =
  let n = norm v in
  if n = 0.0 then zero else scale (1.0 /. n) v
;;

let cross_scalar v1 v2 = (v1.x *. v2.y) -. (v1.y *. v2.x);;
let perp v = make (-.v.y) v.x;;

let dist_sq p1 p2 = norm_sq (sub p1 p2);;
let dist p1 p2 = norm (sub p1 p2);;

let project_on_line v l =
  let l_norm_sq = norm_sq l in
  if l_norm_sq = 0.0 then zero
  else
    let factor = dot v l /. l_norm_sq in
    scale factor l
;;

let to_string v = Printf.sprintf "(%.2f, %.2f)" v.x v.y;;