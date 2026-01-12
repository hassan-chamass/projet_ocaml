open Geometrie


type airplane = {
  id : int;
  pos : vecteur;
  speed : vecteur;
  color : Graphics.color;
  destination : vecteur
}

let create_airplane = fun id pos speed target_pos->
  let r = Random.int 256 in
  let g = Random.int 256 in
  let b = Random.int 256 in
  let color = Graphics.rgb r g b in
  { id=id; pos=pos; speed=speed; color=color; destination=target_pos }

let move_airplane airplane dt =
  let new_pos = {
    x = airplane.pos.x +. airplane.speed.x *. dt;
    y = airplane.pos.y +. airplane.speed.y *. dt;
  } in
  { airplane with pos = new_pos }

