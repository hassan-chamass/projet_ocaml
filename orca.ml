open Geometrie
open Avion



(* -------------------- utilitaires -------------------- *)

let relative_speed = fun a1 a2 ->
  sub a1.speed a2.speed



(* -------------------- géométrie du cône -------------------- *)

(* droites tangentes au cercle (pb, d) passant par pa *)
let droites_cone pa pb d =
  let ax = pa.x and ay = pa.y in
  let bx = pb.x -. ax in
  let by = pb.y -. ay in
  let dist2 = bx *. bx +. by *. by in
  if dist2 <= d *. d then
    failwith "Point dans le cercle"
  else
    let a = bx *. bx -. d *. d in
    let b = -.2. *. bx *. by in
    let c = by *. by -. d *. d in
    let delta = b *. b -. 4. *. a *. c in
    let s = sqrt delta in
    let m1 = (-.b +. s) /. (2. *. a) in
    let m2 = (-.b -. s) /. (2. *. a) in
    let n1 = ay -. m1 *. ax in
    let n2 = ay -. m2 *. ax in
    ({ m = m1; n = n1 }, { m = m2; n = n2 })


let normale_exterieure droite pa pb =
  let v = { x = 1.0; y = droite.m } in
  let n1 = { x = -.v.y; y = v.x } in
  let n2 = { x = v.y; y = -.v.x } in
  let ab = sub pb pa in
  if dot ab n1 > 0. then n1 else n2

(* -------------------- test ORCA -------------------- *)
(*
let v_rel_in_cone = fun a1 a2 ->
  let pa = a1.pos in
  let pb = a2.pos in
  let bp = bprime a1 a2 in
  let droite1, droite2 = droites_cone pa pb d in
  let n1 = normale_exterieure droite1 pa pb in
  let n2 = normale_exterieure droite2 pa pb in
  let v_rel = sub (relative_speed a1 a2) (sub bp pa) in
  not (dot v_rel n1 > 0. || dot v_rel n2 > 0.)*)


let centre_petit_cercle = fun a1 a2 tau->
  let ab = sub a2.pos a1.pos in
  add a1.pos (scale (1. /. tau) ab)



let tangente_petit_cercle a1 a2 d tau =
  let center = centre_petit_cercle a1 a2 tau in
  let r = d /. tau in

  (* vitesse relative *)
  let vr = sub a1.speed a2.speed in
  let p = add a1.pos vr in

  (* direction CP *)
  let cp = sub p center in
  let norm_cp = norm cp in
  if norm_cp = 0. then failwith "vr = centre du cercle"
  else
    let u = scale (1. /. norm_cp) cp in

    (* point de tangence *)
    let t = add center (scale r u) in

    (* direction de la tangente *)
    let dir = { x = -.u.y; y = u.x } in

    (* équation y = m x + n *)
    let m = dir.y /. dir.x in
    let n = t.y -. m *. t.x in
    ({ m; n }, t)
