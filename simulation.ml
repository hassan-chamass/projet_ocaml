open Geometrie
open Avion


let window_width = 1000.0
let window_height = 800.0
let margin = 50.0   (* marge depuis les bords *)
let min_distance = 30.0  (* distance minimale entre avions *)




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

  (*
  (* Génère un cercle de vitesses possibles pour l'échantillonnage *)
/// let get_reachable_speeds = fun vmin vmax dt ->
  let speeds = ref [] in
  for a = 0 to 30 do
    let angle = (float_of_int a) *. (2. *. Float.pi /. 30.) in
    for i = 0 to 10 do
      let c = 
      let v_norm = (float_of_int r) *. c + d in
      speeds := { x = v_norm *. cos angle; y = v_norm *. sin angle } :: !speeds
    done
  done;
  !speeds

let update_airplanes = fun airplanes dt d tau ->
 let reachable_full = get_reachable_speeds () in (* Ri_infinity [cite: 109] *)

  (* Lignes 4-5 : Pour chaque avion, on définit son ensemble de contraintes *)
  let new_states = List.map (fun a_i ->
    let cst_set = List.fold_left (fun acc a_j ->
      if a_i.id = a_j.id then acc
      else (choisir_plan_separateur a_i a_j d tau) :: acc (* Ligne 3 [cite: 128] *)
    ) [] airplanes in

    (* Calcul de v_pref vers la destination de l'avion [cite: 61] *)
    let v_pref = calc_v_pref a_i in

    (* Ligne 6 : Sélection de la vitesse idéale ORCA [cite: 131] *)
    let v_prime_i = select_speed_ORCA cst_set v_pref reachable_full in
    
    (* Ligne 7 & 8 : v_i = Closest(Ri, v'_i) [cite: 132] *)
    let new_v = closest reachable_full v_prime_i in
    
    { a_i with speed = new_v }
  ) airplanes in

  (* Ligne 9 : Déplacement de chaque avion [cite: 133] *)
  List.iter (fun a -> move_airplane a dt) new_states;
  
  new_states (* On retourne l'état mis à jour*)
  *)
