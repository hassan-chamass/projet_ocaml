open Vecteurs
open Avion


let window_width = 900.0
let window_height = 700.0
let min_distance = 30.0  (* distance minimale entre avions au spawn *)



let generate_airplanes = fun n ->
  let c = {x = 500.; y = 300.} in
  let r = 400.0 in
  let delta = 50.0 in
  let theta = ref 0.0 in

  let gen = fun id ->
    let speed_norm = 50.0 +. Random.float 50.0 in
    let pos = {x = c.x +. r *. cos !theta; y = c.y +. r *. sin !theta} in
    let target_pos = { x  = c.x +. (Random.float delta -. delta /. 2.) ; y = c.y +. (Random.float delta -. delta /. 2.) } in

    let direction_vect = sub target_pos pos in
    let speed = scale speed_norm (normalize direction_vect) in
    theta := !theta +. Float.pi /. 4.0 +. Random.float (Float.pi /. 8.0);
    create_airplane id pos speed target_pos
  in
  List.init n (fun i -> gen (i + 1))

  
let rec generate_one_airplane = fun airplanes id ->
  let edge = Random.int 4 in
  let x, y =
    match edge with
    | 0 -> (0.0, Random.float window_height)
    | 1 -> (window_width, Random.float window_height)
    | 2 -> (Random.float window_width, 0.0)
    | _ -> (Random.float window_width, window_height)
  in
  let edge_target = ref (Random.int 4) in
  while!edge_target = edge do
    (edge_target := Random.int 4)
  done;
  let tx, ty =
    match !edge_target with
    | 0 -> (0.0, Random.float window_height)
    | 1 -> (window_width, Random.float window_height)
    | 2 -> (Random.float window_width, 0.0)
    | _ -> (Random.float window_width, window_height)
  in
  let pos = {x = x; y = y} in
  let target_pos = { x = tx ; y = ty } in
  let speed_norm = 50.0 +. Random.float 50.0 in
  let direction_vect = sub target_pos pos in
  let speed = scale speed_norm (normalize direction_vect) in
  let airplane = create_airplane id pos speed target_pos in

  (* Vérifie les collisions avec les avions existants *)
  if List.exists (fun a ->
    (* let ax, ay = a.pos.x a.pos.y in *)
    let d = norm (sub a.pos pos) in
    d < min_distance
  ) airplanes then
    generate_one_airplane airplanes id
  else
    airplane

let in_window = fun airplane ->
  let { x; y } = airplane.pos in
  x >= 0.0 && x <= 1000.0 &&
  y >= 0.0 && y <= 800.0


let update_airplanes = fun airplanes dt ->
  List.iter (fun airplane -> move_airplane airplane dt) airplanes;  
  (* Filtre les avions hors de l'écran *)
  List.filter in_window airplanes
