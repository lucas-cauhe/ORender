open Geometry


type camera = {
  forward: Direction.direction_t;
  left: Direction.direction_t;
  up: Direction.direction_t;
  origin: Point.point_t;
  width: int;
  height: int;
}


let camera up left forward origin (width, height) = {
  up; left; forward; origin;
  width; height;
}

(**
  Returns a point inside a pixel
*)
let point_in_pixel cam (x_, y_) half_width half_height forward_ : Point.point_t = 
  let x_random = Random.float (1. /. float_of_int cam.width) in
  let y_random = Random.float (1. /. float_of_int cam.height) in
  let left_ = Direction.prod cam.left ((x_ +. x_random) *. half_width) in
  let up_ = Direction.prod cam.up ((y_ +. y_random) *. half_height) in
  let dir = Direction.sum left_ up_ |> Direction.sum forward_ in
  Point.from_coords (Direction.x dir)  (Direction.y dir)  (Direction.z dir)


let points_in_pixel cam (row, col) rpp =
  let x_ = 1. -. 2. *. (float_of_int col /. float_of_int cam.width) in
  let y_ = 1. -. 2. *. (float_of_int row /. float_of_int cam.height) in
  let fov = Float.pi /. 2. in
  let ar = float_of_int cam.width /. float_of_int cam.height in
  let distance_to_pplane = 1. in
  let half_height = tan (fov /. 2.) *. distance_to_pplane in
  let half_width = half_height *. ar in
  let forward_ = Direction.prod cam.forward distance_to_pplane in
  BatList.init rpp (fun _ -> point_in_pixel cam (x_, y_) half_width half_height forward_)

let cam_origin cam = cam.origin
