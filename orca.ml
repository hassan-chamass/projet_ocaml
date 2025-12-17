open Geometrie
open Avion

let tau = 1. (*en s temps*)
let d = 30. (*distance minimale*)

let relative_speed = fun a1 a2 ->
  sub a1.speed a2.speed

let bprime = fun a b ->
  let ab = sub b.pos a.pos in
  let abprime = scale (1./.tau) ab in
  add abprime a.pos



let rec orca = fun dt tau airplanes->
  0




let droites_cone = fun a b d ->
  let dx = b.x -. a.x in
  let dy = b.y -. a.y in
  let a_coef = dx *. dx -. d *. d in
  let b_coef = -.2. *. dx *. dy in
  let c_coef = dy *. dy -. d *. d in
  let discriminant = b_coef *. b_coef -. 4. *. a_coef *. c_coef in
  if discriminant < 0. then
    failwith "A est à l'intérieur du cercle, pas de tangentes"
  else
    let sqrt_disc = sqrt discriminant in
    let m1 = (-.b_coef +. sqrt_disc) /. (2. *. a_coef) in
    let m2 = (-.b_coef -. sqrt_disc) /. (2. *. a_coef) in
    let n1 = a.y -. m1 *. a.x in
    let n2 = a.y -. m2 *. a.x in
    ({m = m1; n = n1}, {m = m2; n = n2})


let normale_exterieure = fun droite a b ->
  let v = { x = 1.0; y = droite.m } in
  let n1 = { x = -.v.y; y = v.x } in  (* une normale *)
  let n2 = { x = v.y; y = -.v.x } in  (* l'autre normale *)
  (* choisir celle qui pointe vers l'extérieur du cône *)
  let vec_ba = sub a b in
  if dot vec_ba n1 > 0. then n1 else n2


let v_rel_in_cone = fun a b d ->
  let droite1, droite2 = droites_cone a.pos b.pos d in
  let n1 = normale_exterieure droite1 a.pos b.pos in
  let n2 = normale_exterieure droite2 a.pos b.pos in
  let v_rel = relative_speed a b in
  not (dot v_rel n1 > 0. || dot v_rel n2 > 0.)


