

type airplane = {
  id : int;
  mutable x : float;
  mutable y : float;
  mutable vx : float;
  mutable vy : float;
  color : Graphics.color
}

let create_airplane = fun id x y vx vy ->
  let r = Random.int 256 in
  let g = Random.int 256 in
  let b = Random.int 256 in
  let color = Graphics.rgb r g b in
  { id; x; y; vx; vy; color }

let move_airplane = fun airplane dt ->
    airplane.x <- airplane.x +. airplane.vx *. dt;
    airplane.y <- airplane.y +. airplane.vy *. dt

let change_speed = fun airplane new_vx new_vy ->
    airplane.vx <- new_vx;
    airplane.vy <- new_vy

let get_position = fun airplane ->
    (airplane.x, airplane.y)

let get_speed = fun airplane ->
    (airplane.vx, airplane.vy)