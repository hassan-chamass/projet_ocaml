

type vecteur = {
  x : float;
  y : float
}

type droite = { 
  m: float;
  n: float 
}

type contrainte = { 
  n : vecteur; 
  point : vecteur; 
}

let create_vector = fun x y ->
  {x=x; y=y}

let v_zero = fun () ->
  create_vector 0. 0.

let add = fun v1 v2 ->
  create_vector (v1.x +. v2.x) (v1.y +. v2.y)

let sub = fun v1 v2 ->
  create_vector (v1.x -. v2.x) (v1.y -. v2.y)

let scale = fun s v ->
  create_vector (s *. v.x) (s *. v.y)

let dot = fun v1 v2 ->
  (v1.x *. v2.x) +. (v1.y *. v2.y)

let norm = fun v ->
  sqrt (dot v v)



let normalize = fun v ->
  let n = norm v in
  if n = 0. then v_zero ()
  else scale (1. /. n) v


let creation_contrainte n point =
  (* crée un objet de type contrainte à partir d'un point et d'une normale *)
  { n = normalize n; point = point}

let angle_from_vector = fun v ->
  atan2 v.y v.x