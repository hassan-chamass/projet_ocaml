open Geometrie
open Avion


let vmax = 120.0 (*Vitesse max et min des avions*)
let vmin = 40.0
let nb_angle_samples = 30 
let nb_speed_samples = 10 


(* -------------------- utilitaires -------------------- *)

let relative_speed = fun a1 a2 ->
  sub a1.speed a2.speed

let calc_v_pref = fun a ->
  let dir = sub a.destination a.pos in
  let dir_normalised = normalize dir in
  scale (vmax) dir_normalised

(* -------------------- géométrie du cône -------------------- *)

let droites_cone pa pb d =
  (* droites tangentes au cercle (pb, d) passant par pa *)
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


let normale_cone_exterieure droite pa pb =
  let v = { x = 1.0; y = droite.m } in
  let n1 = { x = -.v.y; y = v.x } in
  let n2 = { x = v.y; y = -.v.x } in
  let ab = sub pb pa in
  if dot ab n1 > 0. then n1 else n2

let normale_tangente_exterieure t centre =
  (* rayon au point de tangence *)
  let r = sub t centre in
  normalize r



(* -------------------- test ORCA -------------------- *)



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

let closest = fun s v_pref ->
  match s with
  | [] -> failwith "Liste vide dans closest"
  | h :: t ->
      List.fold_left (fun acc v ->
        (* if norm (sub v v_pref) <= norm (sub acc v_pref) then v else acc *)
        (* on normalise les vecteurs puis on prend le plus grand produit scalaire pour avoir l'angle le plus proche*)
        if dot (normalize v) (normalize v_pref) >= dot (normalize acc) (normalize v_pref) then v else acc
      ) h t

let violation_contrainte = fun v c ->
  dot c.n (sub v c.point)

let droite_cone_plus_proche vr pa pb d =
  (* Trouve la contrainte la plus proche de vr (quand le point coté négatif tangeante)*)
  let d1, d2 = droites_cone pa pb d in
  let n1 = normalize (normale_cone_exterieure d1 pa pb) in
  let n2 = normalize (normale_cone_exterieure d2 pa pb) in
  let c1 = {n = n1 ; point = pa} in
  let c2 = {n = n2 ; point = pa} in
  let v1 = violation_contrainte vr c1 in
  let v2 = violation_contrainte vr c2 in

  if v1 > v2 then (d1, n1) else (d2, n2)


let point_cote_positif = fun a contrainte ->
  (* test si le point A (type vecteur) est positif pour une droite *)
  dot contrainte.n (sub a contrainte.point) >= 0.


let evalCst = fun cst v ->
  dot cst.n (sub v cst.point)


let choisir_plan_separateur a_i a_j d tau =
  let vr = relative_speed a_i a_j in
  let diff_pos = sub a_i.pos a_j.pos in
  let dist2 = dot diff_pos diff_pos in

  (* CAS 1 : avions trop proches → contrainte radiale *)
  if dist2 <= d *. d then
    let n = normalize diff_pos in
    creation_contrainte n vr

  (* CAS 2 : ORCA normal *)
  else
    let centre = centre_petit_cercle a_i a_j tau in
    let (_, t) = tangente_petit_cercle a_i a_j d tau in
    let n_t = normale_tangente_exterieure t centre in
    let contr = creation_contrainte n_t t in

    if evalCst contr vr >= 0. then
      contr
    else
      let pa = a_i.pos in
      let pb = a_j.pos in
      let (_, n_c) = droite_cone_plus_proche vr pa pb d in
      creation_contrainte n_c pa



let empty_ORCA = fun reachable_speeds cst_set ->
  (* Algorithme 3 *)
  let best_v = ref (List.hd reachable_speeds) in
  let max_min_val = ref Float.neg_infinity in
  List.iter (fun v_test ->
    let current_min = List.fold_left (fun acc cst ->
      let s = evalCst cst v_test in
      if s < acc then s else acc
    ) Float.max_float cst_set in
    if current_min > !max_min_val then (
      max_min_val := current_min;
      best_v := v_test
    )
  ) reachable_speeds;
  !best_v


  
let reachable_speeds_inf () =
  let rec angles i acc =
    if i >= nb_angle_samples then acc
    else
      let a = 2. *. Float.pi *. float i /. float nb_angle_samples in
      angles (i + 1) (a :: acc)
  in

  let rec speeds i acc =
    if i > nb_speed_samples then acc
    else
      let s =
        vmin
        +. (vmax -. vmin) *. float i /. float nb_speed_samples
      in
      speeds (i + 1) (s :: acc)
  in

  let angles = angles 0 [] in
  let speeds = speeds 0 [] in

  List.concat (
    List.map (fun a ->
      List.map (fun s ->
        { x = s *. cos a; y = s *. sin a }
      ) speeds
    ) angles
  )


let reachable_speeds = fun avion dt ->
  (*reachable speed with time infinity*)
  let max_turn_rate = 10. *. Float.pi /. 180. in (* 10°/s *)
  let max_accel = 20.0  in

  let speed_norm = norm avion.speed in
  let speed_angle = angle_from_vector avion.speed in

  let dtheta = max_turn_rate *. dt in
  let dv = max_accel *. dt in

  let max_speed = min vmax (speed_norm +. dv) in
  let min_speed = max vmin (speed_norm -. dv) in

  (* fonction recurcive pour remplacer boucle for *)
  let rec angles i acc =
    if i > nb_angle_samples then acc
    else
      let a =
        speed_angle -. dtheta
        +. (2. *. dtheta *. float i /. float nb_angle_samples)
      in
      angles (i + 1) (a :: acc)
  in

  let rec speeds i acc =
    if i > nb_speed_samples then acc
    else
      let s =
        min_speed +. (max_speed -. min_speed) *. float i /. float nb_speed_samples
      in
      speeds (i + 1) (s :: acc)
  in

  let angles = angles 0 [] in
  let speeds = speeds 0 [] in

  List.concat (
    List.map (fun a ->
      List.map (fun s ->
        { x = s *. cos a; y = s *. sin a }
      ) speeds
    ) angles
  )

let select_speed_ORCA = fun cst_set v_pref ->
  let reachable_speeds = reachable_speeds_inf () in
  let s_i = List.filter (fun v_test ->
    List.for_all (fun cst -> (evalCst cst v_test) >= 0.) cst_set
  ) reachable_speeds in
  match s_i with
  | [] -> empty_ORCA reachable_speeds cst_set
  | _ -> closest s_i v_pref
