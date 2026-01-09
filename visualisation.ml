open Simulation
open Avion
open Geometrie
open Orca
let n_airplanes = 2
let tau = 5.0 
let d = 60.0
let dt = 0.01
let debug = false


let draw_airplane = fun airplane ->
  let x, y = airplane.pos.x, airplane.pos.y in
  Graphics.set_color airplane.color;

  (* Dessine le cercle de l'avion *)
  Graphics.fill_circle (int_of_float x) (int_of_float y) 5;
  
  (* Calcul de la flèche *)
  let arrow_head = add airplane.pos airplane.speed in
  let x_end, y_end = arrow_head.x, arrow_head.y in
  
  
  (* Dessine la ligne/flèche *)
  Graphics.moveto (int_of_float x) (int_of_float y);
  Graphics.lineto (int_of_float x_end) (int_of_float y_end);

  (* rayon de sécurité *)
  let radius = int_of_float d in

  (* dessiner le cercle autour de a*)
  Graphics.set_color Graphics.red;
  Graphics.draw_circle
    (int_of_float airplane.pos.x)
    (int_of_float airplane.pos.y)
    radius



  (* Dessine un bouton simple *)
let draw_button = fun x y w h label color ->
  Graphics.set_color color;
  Graphics.fill_rect x y w h;
  Graphics.set_color Graphics.black;
  Graphics.moveto (x + 10) (y + h / 2 - 5);
  Graphics.draw_string label

                  (* Dessins pour ORCA *)
let draw_droite_orientee = fun droite pa pb length color ->
  Graphics.set_color color;

  (* direction du cône *)
  let v = sub pb pa in
  let norm = sqrt (dot v v) in
  let d = scale (1. /. norm) v in

  (* vecteur directeur de la droite *)
  let v = { x = 1.0; y = droite.m } in

  (* choisir le sens qui va vers le cercle *)
  let v =
    if dot v d < 0. then scale (-1.) v else v
  in

  let x1 = pa.x +. length *. v.x in
  let y1 = pa.y +. length *. v.y in

  Graphics.moveto (int_of_float pa.x) (int_of_float pa.y);
  Graphics.lineto (int_of_float x1) (int_of_float y1)

let draw_relative_velocity a1 a2 =
  let start = a1.pos in
  let vr = relative_speed a1 a2 in
  let endp = add start vr in

  Graphics.set_color Graphics.magenta;
  Graphics.moveto
    (int_of_float start.x)
    (int_of_float start.y);
  Graphics.lineto
    (int_of_float endp.x)
    (int_of_float endp.y)

let draw_tangente_petit_cercle = fun a1 a2 d tau ->
  let (droite, t) = tangente_petit_cercle a1 a2 d tau in

  let v = normalize { x = 1.0; y = droite.m } in
  let length = 100.0 in

  let p1 = add t (scale length v) in
  let p2 = add t (scale (-.length) v) in

  Graphics.set_color Graphics.cyan;
  Graphics.moveto
    (int_of_float p1.x)
    (int_of_float p1.y);
  Graphics.lineto
    (int_of_float p2.x)
    (int_of_float p2.y)



let draw_cone = fun a1 a2 ->
  let pa = a1.pos in
  let pb = a2.pos in

  (* rayon de sécurité *)
  let radius = int_of_float d in

  (* dessiner le cercle autour de a2 *)
  Graphics.set_color Graphics.red;
  Graphics.draw_circle
    (int_of_float pb.x)
    (int_of_float pb.y)
    radius;

  (* dessiner le petit cercle *)
  let center = centre_petit_cercle a1 a2 tau in
  Graphics.set_color Graphics.blue;
  Graphics.draw_circle
    (int_of_float (center.x))
    (int_of_float (center.y))
    (int_of_float (d /. tau));

  (* calcul des tangentes *)
  try
    let d1, d2 = droites_cone pa pb d in
    draw_droite_orientee d1 pa pb 1000.0 Graphics.green;
    draw_droite_orientee d2 pa pb 1000.0 Graphics.green;
    draw_relative_velocity a1 a2;
    draw_tangente_petit_cercle a1 a2 d tau
  with _ ->
    ()
  
            (* fin dessins ORCA *)

  
let draw_dest = fun airplane ->
  Graphics.moveto (int_of_float airplane.pos.x) (int_of_float airplane.pos.y);
  Graphics.lineto (int_of_float airplane.destination.x) (int_of_float airplane.destination.y)

let draw_all = fun airplanes random_add paused ->
  Graphics.clear_graph ();
  (* Dessine les boutons *)
  draw_button 10 700 100 30 "Add Plane" Graphics.yellow;
  draw_button 120 700 150 30 ("Random Add: " ^ (if random_add then "ON" else "OFF")) Graphics.cyan;
  draw_button 280 700 100 30 "Restart" Graphics.red;
  draw_button 390 700 120 30 (if paused then "Play" else "Pause") Graphics.green;
  (* Dessine les avions *)
  List.iter draw_airplane airplanes;

   (* VISUALISATION DU CÔNE *)
  (match airplanes with
   | a1 :: a2 :: _ -> draw_cone a1 a2
   | _ -> ());

  (match airplanes with
    | a1 :: _ -> draw_dest a1
    | _ -> ());
  Graphics.synchronize ()


(* Vérifie si le point (x,y) est dans le rectangle (bx,by,w,h) *)
let is_inside = fun x y bx by bw bh ->
  x >= bx && x <= bx + bw && y >= by && y <= by + bh


let print_airplanes airplanes =
  Printf.printf "Airplanes ids: ";
  List.iter (fun a -> Printf.printf "%d " a.id) airplanes;
  print_endline ""



let rec loop = fun airplanes current_id random_add paused ->
  (* Met à jour la simulation *)
  let airplanes =
    if paused then airplanes
    else update_airplanes airplanes dt d tau
  in
  if debug then print_airplanes airplanes;  (* <-- vérifie ici *)
  Unix.sleepf 0.01;

  (* Spawn automatique si activé *)
  let airplanes, new_id =
    if random_add && Random.float 1.0 < 0.01 then
      let new_plane = generate_one_airplane airplanes (current_id+1) in
      (new_plane :: airplanes, current_id + 1)
    else
      (airplanes, current_id)
  in

  (* Dessine tout *)
  draw_all airplanes random_add paused;

  (* Vérifie les clics *)
  if Graphics.button_down () then (
    let x, y = Graphics.mouse_pos () in
    (* Ajout manuel *)
    if is_inside x y 10 700 100 30 then(
      let new_plane = generate_one_airplane airplanes (new_id+1) in
      (* Attend que le bouton soit relâché pour éviter les multiples clics *)
      while Graphics.button_down () do () done;
      loop (new_plane :: airplanes) (new_id + 1) random_add paused
    )
    (* Toggle random add *)
    else if is_inside x y 120 700 150 30 then(
      while Graphics.button_down () do () done;
      loop airplanes new_id (not random_add) paused
    )
    (* Restart *)
    else if is_inside x y 280 700 100 30 then(
      while Graphics.button_down () do () done;
      let airplanes = generate_airplanes n_airplanes in
      loop airplanes n_airplanes random_add paused
    )
    (* Pause/Play *)
    else if is_inside x y 390 700 120 30 then(
      while Graphics.button_down () do () done;
      loop airplanes new_id random_add (not paused)
    )
    else
      loop airplanes new_id random_add paused
  )
  else
    loop airplanes new_id random_add paused



let () =
  Random.self_init ();
  Graphics.open_graph " 1000x800";
  Graphics.auto_synchronize false;

  let airplanes = generate_airplanes n_airplanes in
  loop airplanes n_airplanes false false


(*
Commandes pour lancer:

opam switch create graphics 4.14.1
opam install graphics
opam switch graphics
opam install ocamlfind

(& opam env) -split '\r?\n' | ForEach-Object { Invoke-Expression $_ }

ocamlfind ocamlc -o visualisation.exe -package graphics,unix -linkpkg vecteurs.ml avion.ml simulation.ml visualisation.ml 
.\visualisation.exe

*)

