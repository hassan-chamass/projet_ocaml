open Simulation
open Avion
open Vecteurs
let n_airplanes = 3 



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
  Graphics.lineto (int_of_float x_end) (int_of_float y_end)



  (* Dessine un bouton simple *)
let draw_button = fun x y w h label color ->
  Graphics.set_color color;
  Graphics.fill_rect x y w h;
  Graphics.set_color Graphics.black;
  Graphics.moveto (x + 10) (y + h / 2 - 5);
  Graphics.draw_string label



let draw_all = fun airplanes random_add ->
  Graphics.clear_graph ();
  (* Dessine les boutons *)
  draw_button 10 700 100 30 "Add Plane" Graphics.yellow;
  draw_button 120 700 150 30 ("Random Add: " ^ (if random_add then "ON" else "OFF")) Graphics.cyan;
  draw_button 280 700 100 30 "Restart" Graphics.red;
  (* Dessine les avions *)
  List.iter draw_airplane airplanes;
  Graphics.synchronize ()


(* Vérifie si le point (x,y) est dans le rectangle (bx,by,w,h) *)
let is_inside = fun x y bx by bw bh ->
  x >= bx && x <= bx + bw && y >= by && y <= by + bh


let rec loop = fun airplanes current_id random_add ->
  (* Met à jour la simulation *)
  update_airplanes airplanes 0.01;
  Unix.sleepf 0.05;

  (* Spawn automatique si activé *)
  let airplanes, next_id =
    if random_add && Random.float 1.0 < 0.01 then
      let new_plane = generate_one_airplane airplanes current_id in
      (new_plane :: airplanes, current_id + 1)
    else
      (airplanes, current_id)
  in

  (* Dessine tout *)
  draw_all airplanes random_add;

  (* Vérifie les clics *)
  if Graphics.button_down () then (
    let x, y = Graphics.mouse_pos () in
    (* Ajout manuel *)
    if is_inside x y 10 700 100 30 then(
      let new_plane = generate_one_airplane airplanes next_id in
      (* Attend que le bouton soit relâché pour éviter les multiples clics *)
      while Graphics.button_down () do () done;
      loop (new_plane :: airplanes) (next_id + 1) random_add
    )
    (* Toggle random add *)
    else if is_inside x y 120 700 150 30 then(
      while Graphics.button_down () do () done;
      loop airplanes next_id (not random_add)
    )
    (* Restart *)
    else if is_inside x y 280 700 100 30 then(
      while Graphics.button_down () do () done;
      let airplanes = generate_airplanes n_airplanes in
      loop airplanes n_airplanes random_add
    )
    else
      loop airplanes next_id random_add
  )
  else
    loop airplanes next_id random_add



let () =
  Random.self_init ();
  Graphics.open_graph " 1000x800";
  Graphics.auto_synchronize false;

  let airplanes = generate_airplanes n_airplanes in
  loop airplanes n_airplanes false


(*
Commandes pour lancer:

opam switch create graphics 4.14.1
opam install graphics
opam switch graphics
opam install ocamlfind

(& opam env) -split '\r?\n' | ForEach-Object { Invoke-Expression $_ }

ocamlfind ocamlc -o visualisation.exe -package graphics,unix -linkpkg avion.ml simulation.ml visualisation.ml
.\visualisation.exe

*)