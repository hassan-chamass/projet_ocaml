open Geometrie
open Avion
open Orca

let window_width = 1000.0
let window_height = 800.0
let margin = 50.0   (* marge depuis les bords *)
let min_distance = 50.0  (* distance minimale entre avions *)
let safe_distance = min_distance +. 10.0



  
let rec generate_one_airplane = fun airplanes id ->
  let edge = Random.int 4 in
  let x, y =
    match edge with
    | 0 -> (margin, margin +. Random.float (window_height -. 2.0 *. margin))   (* gauche *)
    | 1 -> (window_width -. margin, margin +. Random.float (window_height -. 2.0 *. margin))  (* droite *)
    | 2 -> (margin +. Random.float (window_width -. 2.0 *. margin), margin)   (* haut *)
    | _ -> (margin +. Random.float (window_width -. 2.0 *. margin), window_height -. margin)  (* bas *)
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
    d < safe_distance
  ) airplanes then
    generate_one_airplane airplanes id
  else
    airplane


let generate_airplanes = fun n ->
  (* Initialise la simulation *)
  List.init n (fun i -> generate_one_airplane [] (i+1))



let in_window = fun airplane ->
  (* Vérifie si un avion est toujours dans la fenêtre *)
  let { x; y } = airplane.pos in
  x >= 0.0 && x <= 1000.0 &&
  y >= 0.0 && y <= 800.0



let update_airplanes = fun airplanes dt d tau ->

  (* Pour chaque avion, on définit son ensemble de contraintes *)
  let new_states = List.map (fun a_i ->
    (* On prends les contraintes pour chaque couple d'avion *)
    let cst_set = List.fold_left (fun acc a_j ->
      if a_i.id = a_j.id then acc
      else (choisir_plan_separateur a_i a_j d tau) :: acc 
    ) [] airplanes in

    (* Calcul de v_pref vers la destination de l'avion *)
    let v_pref = calc_v_pref a_i in

    (* Sélection de la vitesse idéale ORCA *)
    let v_prime_i = select_speed_ORCA cst_set v_pref in

    let reachable = reachable_speeds a_i dt in
    
    (*  v_i = Closest(Ri, v'_i)  *)
    let new_v = closest reachable v_prime_i in
    
    { a_i with speed = new_v }
  ) airplanes in

  (* Déplacement de chaque avion *)
  let moved = List.map (fun a -> move_airplane a dt) new_states in
  List.filter in_window moved
