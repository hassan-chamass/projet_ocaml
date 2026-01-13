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
  (* normale extérieure au cône défini par la droite et les points pa et pb *)
  let v = { x = 1.0; y = droite.m } in
  let n1 = { x = -.v.y; y = v.x } in
  let n2 = { x = v.y; y = -.v.x } in
  let ab = sub pb pa in
  let n = if dot ab n1 < 0. then n1 else n2 in
  normalize n



let normale_tangente_exterieure t centre =
  (* rayon au point de tangence *)
  let r = sub t centre in
  normalize r


(* -------------------- test ORCA -------------------- *)


let evalCst = fun cst v ->
  dot cst.n (sub v cst.point)


let v_rel_in_cone = fun a1 a2 d ->
  let pa = a1.pos in
  let pb = a2.pos in
  let vr = relative_speed a1 a2 in
  
  let droite1, droite2 = droites_cone pa pb d in
  let n1 = normale_cone_exterieure droite1 pa pb in
  let n2 = normale_cone_exterieure droite2 pa pb in
  
  (* vr est dans le cône si les deux produits scalaires sont négatifs *)
  (* (i.e. vr est du côté intérieur des deux droites) *)
  let cst1 = creation_contrainte n1 pa in
  let cst2 = creation_contrainte n2 pa in
  
  (evalCst cst1 (add pa vr) < 0.) && (evalCst cst2 (add pa vr) < 0.)



let centre_petit_cercle = fun a1 a2 tau->
  let ab = sub a2.pos a1.pos in
  add a1.pos (scale (1. /. tau) ab)



let tangente_petit_cercle a1 a2 d tau =
  let center = centre_petit_cercle a1 a2 tau in
  let r = d /. tau in

  (* vitesse relative *)
  let vr = relative_speed a1 a2 in
  let p = add a1.pos vr in

  (* direction CP *)
  let cp = sub p center in
  let norm_cp = norm cp in
  if norm_cp = 0. then failwith "vr = centre du cercle"
  else
    let u = normalize cp in

    (* point de tangence *)
    let t = add center (scale r u) in

    (* direction de la tangente *)
    let dir = { x = -.u.y; y = u.x } in

    (* équation y = m x + n *)
    let m = dir.y /. dir.x in
    let n = t.y -. m *. t.x in
    ({ m; n }, t)


(*
let closest = fun s v_pref ->
  match s with
  | [] -> failwith "Liste vide dans closest"
  | h :: t ->
      List.fold_left (fun acc v ->
        (* on normalise les vecteurs puis on prend le plus grand produit scalaire pour avoir l'angle le plus proche*)
        if dot (normalize v) (normalize v_pref) >= dot (normalize acc) (normalize v_pref) then v else acc
      ) h t
*)



let closest = fun s v_pref ->
  match s with
  | [] -> failwith "Liste vide dans closest"
  | h :: t ->
      List.fold_left (fun acc v ->
        let dist_v = norm (sub v v_pref) in
        let dist_acc = norm (sub acc v_pref) in
        if dist_v < dist_acc then v else acc
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




let choisir_plan_separateur a_i a_j d tau =
  let vr = relative_speed a_i a_j in
  let diff_pos = sub a_i.pos a_j.pos in
  let dist2 = dot diff_pos diff_pos in

  (* CAS 1 : avions trop proches → contrainte radiale *)
  if dist2 <= d *. d then
    let n = normalize diff_pos in
    creation_contrainte n a_j.speed

  (* CAS 2 : ORCA normal *)
  else
    let centre = centre_petit_cercle a_i a_j tau in
    let (_, t) = tangente_petit_cercle a_i a_j d tau in
    let n_t = normale_tangente_exterieure t centre in
    let contr = creation_contrainte n_t a_i.speed in
    
    let pa = a_i.pos in    
    let pos_vr = add pa vr in
    
       
    if evalCst contr pos_vr >= 0. then
      contr
      
    else 
      let pa = a_i.pos in
      let pb = a_j.pos in
      
      if v_rel_in_cone a_i a_j d then
        (* vr DANS le cône : projection de pos_vr sur la droite *)
        let (droite_proche, n_c) = droite_cone_plus_proche vr pa pb d in
        let m = droite_proche.m in
        let n_droite = droite_proche.n in
        
        (* Position de vr dans l'espace *)
        let pos_vr = add pa vr in
        
        (* Projection orthogonale de pos_vr sur la droite y = mx + n *)
        let x_proj = (pos_vr.x +. m *. pos_vr.y -. m *. n_droite) /. (1. +. m *. m) in
        let y_proj = m *. x_proj +. n_droite in
        let point_proj = { x = x_proj; y = y_proj } in
        
        (* c_tau = vecteur de pos_vr vers point_proj *)
        let c_tau = sub point_proj pos_vr in
        
        (* Point de contrainte = v_A + c_tau/2 *)
        let point_contrainte = add a_i.speed (scale 0.5 c_tau) in
        
        creation_contrainte n_c point_contrainte
        
      else
        (* vr HORS du cône : projection de v_A sur la droite *)
        let (droite_proche, n_c) = droite_cone_plus_proche vr pa pb d in
        let m = droite_proche.m in
        let n_droite = droite_proche.n in
        
        (* Projection de v_A sur la droite *)
        let x_proj = (a_i.speed.x +. m *. a_i.speed.y -. m *. n_droite) /. (1. +. m *. m) in
        let y_proj = m *. x_proj +. n_droite in
        let point_proj = { x = x_proj; y = y_proj } in
        
        creation_contrainte n_c point_proj
(*
let choisir_plan_separateur a_i a_j d tau =
  let vr = relative_speed a_i a_j in
  let diff_pos = sub a_i.pos a_j.pos in
  let dist2 = dot diff_pos diff_pos in

   Printf.printf "Avion %d vs %d: dist=%.1f, vr_norm=%.1f\n" 
    a_i.id a_j.id (sqrt dist2) (norm vr);
  (* CAS 1 : avions trop proches → contrainte radiale *)
  if dist2 <= d *. d then
    let n = normalize diff_pos in
    creation_contrainte n a_j.speed

  (* CAS 2 : ORCA normal *)
  else
    let centre = centre_petit_cercle a_i a_j tau in
    let (droite, t) = tangente_petit_cercle a_i a_j d tau in
    let n_t = normale_tangente_exterieure t centre in
    
    let vr_projected = sub t a_i.pos in
    let c_tau = sub vr_projected vr in
    
    let point_contrainte = add a_i.speed (scale 0.5 c_tau) in
    let contr = creation_contrainte n_t point_contrainte in

    if evalCst contr vr >= 0. then
      contr
    else
      let pa = a_i.pos in
      let pb = a_j.pos in
      let (_, n_c) = droite_cone_plus_proche vr pa pb d in
      creation_contrainte n_c a_i.speed
*)

(*
let choisir_plan_separateur a_i a_j d tau =
  let vr = relative_speed a_i a_j in
  let diff_pos = sub a_i.pos a_j.pos in
  let dist2 = dot diff_pos diff_pos in

  Printf.printf "Avion %d vs %d: dist=%.1f, vr_norm=%.1f\n" 
    a_i.id a_j.id (sqrt dist2) (norm vr);

  (* CAS 1 : avions trop proches → contrainte radiale *)
  if dist2 <= d *. d then
    let n = normalize diff_pos in
    (* Contrainte radiale : la vitesse doit être telle qu'on s'éloigne *)
    creation_contrainte n a_i.speed

  (* CAS 2 : ORCA normal *)
  else
    let centre = centre_petit_cercle a_i a_j tau in
    let rayon = d /. tau in
    
    (* Position de vr dans l'espace des vitesses relatives *)
    let pos_vr_abs = add a_i.pos vr in  (* position absolue de vr *)
    let dir_centre_vers_vr = sub pos_vr_abs centre in
    let dist_centre_vr = norm dir_centre_vers_vr in
    
    (* Vérifier si vr est DANS la zone interdite δ^-_τ *)
    if dist_centre_vr < rayon then
      (* vr est DANS la zone interdite *)
      (* On projette vr sur le bord du cercle *)
      let u = scale (1. /. dist_centre_vr) dir_centre_vers_vr in
      let t = add centre (scale rayon u) in  (* point projeté sur δ_τ *)
      
      (* c_τ = vecteur de vr (absolu) vers son projeté t *)
      let c_tau = sub t pos_vr_abs in
      
      (* Normale extérieure = direction radiale au point t *)
      let n_t = normalize u in
      
      (* Point de contrainte = v_A + c_τ/2 selon le sujet *)
      let point_contrainte = add a_i.speed (scale 0.5 c_tau) in
      
      creation_contrainte n_t point_contrainte
      
    else
      (* vr est HORS de la zone interdite *)
      (* Il faut quand même créer une contrainte pour éviter d'y entrer *)
      (* On utilise la tangente au cercle depuis vr *)
      
      (* Trouver la droite du cône la plus proche de vr *)
      let pa = a_i.pos in
      let pb = a_j.pos in
      
      try
        let (_, n_c) = droite_cone_plus_proche vr pa pb d in
        (* Le point de contrainte est v_A car pas de correction nécessaire *)
        creation_contrainte n_c a_i.speed
      with _ ->
        (* En cas d'erreur (géométrie dégénérée), contrainte radiale *)
        let n = normalize diff_pos in
        creation_contrainte n a_i.speed
*)





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
  (*reachable speed without limit*)
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
  (*reachable speed with time dt*)
  let max_turn_rate = 20. *. Float.pi /. 180. in (* 10°/s *)
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
