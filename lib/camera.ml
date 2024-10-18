type camera = {
  forward: Geometry.Direction.t;
  left: Geometry.Direction.t;
  up: Geometry.Direction.t;
  origin: Geometry.Point.t;
  width: int;
  height: int;
}

let camera up left forward origin = {
  up; left; forward; origin;
  width = 256; height = 256;
}

(* let point_in_pixel cam (row, col) =
  let open Geometry in
  let left = Direction.prod cam.left (1. -. float_of_int row /. (float_of_int cam.width /. 2.)) in
  let up = Direction.prod cam.up (1. -. float_of_int col /. (float_of_int cam.height /. 2.)) in
  (* Geometry.Point.from_coords x y ((Point.z cam.origin) +. (Direction.z cam.forward)) *)
  let dir = cam.origin |> Direction.of_point |> Direction.sum cam.forward |> Direction.sum left |> Direction.sum up in
  (* let dir = Direction.sum (Direction.sum (Direction.sum (Direction.of_point cam.origin) cam.forward) left) up in *)
  Point.from_coords (Direction.x dir)  (Direction.y dir)  (Direction.z dir) *)

let point_in_pixel cam (row, col) = 
  let open Geometry in
  let x_ = 2. *. (float_of_int col /. float_of_int cam.width) -. 1. in
  let y_ = 1. -. 2. *. (float_of_int row /. float_of_int cam.height) in
  let fov = Float.pi /. 2. in
  let distance_to_pplane = 1. in
  let ar = float_of_int cam.width /. float_of_int cam.height in
  let half_height = tan (fov /. 2.) *. distance_to_pplane in
  let half_width = half_height *. ar in
  let left_ = Direction.prod cam.left (x_ *. half_width) in
  let up_ = Direction.prod cam.up (y_ *. half_height) in
  let forward_ = Direction.prod cam.forward distance_to_pplane in
  let dir = Direction.sum left_ up_ |> Direction.sum forward_ in
  Point.from_coords (Direction.x dir)  (Direction.y dir)  (Direction.z dir) 

 
let trace_ray cam (row, col) scene pool =
  let ray_direction = Geometry.Direction.between_points (point_in_pixel cam (row, col)) cam.origin in
  let ray = Figures.ray cam.origin ray_direction in
  match Figures.find_closest_figure scene ray pool with 
  | Some(fig) -> Figures.emission fig
  | None -> Colorspace.Rgb.rgb_of_values 0. 0. 0.