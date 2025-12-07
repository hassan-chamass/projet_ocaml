
open Avion


let window_width = 900.0
let window_height = 700.0
let min_distance = 30.0  (* distance minimale entre avions au spawn *)



let generate_airplanes = fun n ->
  let cx = 500.0 and cy = 300.0 in
  let r = 400.0 in
  let delta = 50.0 in
  let theta = ref 0.0 in

  let gen = fun id ->
    let speed = 50.0 +. Random.float 50.0 in
    let x = cx +. r *. cos !theta in
    let y = cy +. r *. sin !theta in
    let tx = cx +. (Random.float delta -. delta /. 2.) in
    let ty = cy +. (Random.float delta -. delta /. 2.) in
    let dx = tx -. x in
    let dy = ty -. y in
    let dist = sqrt (dx *. dx +. dy *. dy) in
    let vx = dx /. dist *. speed in
    let vy = dy /. dist *. speed in
    theta := !theta +. Float.pi /. 4.0 +. Random.float (Float.pi /. 8.0);
    create_airplane id x y vx vy
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
  let tx = Random.float window_width in
  let ty = Random.float window_height in
  let dx = tx -. x in
  let dy = ty -. y in
  let dist = sqrt (dx *. dx +. dy *. dy) in
  let speed = 50.0 +. Random.float 50.0 in
  let vx = dx /. dist *. speed in
  let vy = dy /. dist *. speed in
  let airplane = create_airplane id x y vx vy in

  (* Vérifie les collisions avec les avions existants *)
  if List.exists (fun a ->
    let ax, ay = get_position a in
    let d = sqrt ((ax -. x) ** 2. +. (ay -. y) ** 2.) in
    d < min_distance
  ) airplanes then
    generate_one_airplane airplanes id
  else
    airplane



let update_airplanes = fun airplanes dt ->
  List.iter (fun airplane -> move_airplane airplane dt) airplanes

