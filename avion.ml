open Vecteurs


type airplane = {
  id : int;
  pos : vecteur;
  speed : vecteur;
  color : Graphics.color
}

let create_airplane = fun id pos speed->
  let r = Random.int 256 in
  let g = Random.int 256 in
  let b = Random.int 256 in
  let color = Graphics.rgb r g b in
  { id=id; pos=pos; speed=speed; color=color }

let move_airplane = fun airplane dt ->
    airplane.pos.x <- airplane.pos.x +. airplane.speed.x *. dt;
    airplane.pos.y <- airplane.pos.y +. airplane.speed.y *. dt

let change_speed = fun airplane new_vx new_vy ->
    airplane.speed.x <- new_vx;
    airplane.speed.y <- new_vy