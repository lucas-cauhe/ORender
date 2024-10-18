open Geometry

type camera = {
  forward: Geometry.Direction.t;
  left: Geometry.Direction.t;
  up: Geometry.Direction.t;
  origin: Geometry.Point.t;
  width: int;
  height: int;
}

let num_points = ref 128 

let camera up left forward origin = {
  up; left; forward; origin;
  width = 256; height = 256;
}

let point_in_pixel cam (x_, y_) half_width half_height forward_ : Point.t = 
  let x_random = Random.float (1. /. float_of_int cam.width) in
  let y_random = Random.float (1. /. float_of_int cam.height) in
  let left_ = Direction.prod cam.left ((x_ +. x_random) *. half_width) in
  let up_ = Direction.prod cam.up ((y_ +. y_random) *. half_height) in
  let dir = Direction.sum left_ up_ |> Direction.sum forward_ in
  Point.from_coords (Direction.x dir)  (Direction.y dir)  (Direction.z dir)


let points_in_pixel cam (row, col) : Point.t BatList.t =
  let x_ = 1. -. 2. *. (float_of_int col /. float_of_int cam.width) in
  let y_ = 1. -. 2. *. (float_of_int row /. float_of_int cam.height) in
  let fov = Float.pi /. 2. in
  let ar = float_of_int cam.width /. float_of_int cam.height in
  let distance_to_pplane = 1. in
  let half_height = tan (fov /. 2.) *. distance_to_pplane in
  let half_width = half_height *. ar in
  let forward_ = Direction.prod cam.forward distance_to_pplane in
  BatList.init !num_points (fun _ -> point_in_pixel cam (x_, y_) half_width half_height forward_)


let trace_ray scene pool ray : Colorspace.Rgb.pixel =
  match Figures.find_closest_figure scene ray pool with 
  | Some(fig) -> Figures.emission fig
  | None -> Colorspace.Rgb.rgb_of_values 0. 0. 0.

 
let pixel_color cam (row, col) scene pool =
  let open Colorspace in
  let color_list = BatList.map (fun dir -> Direction.between_points dir cam.origin |> Figures.ray cam.origin |> trace_ray scene pool) (points_in_pixel cam (row, col)) in
  let color_sum = BatList.fold_left (fun acc next -> Rgb.sum acc next) (Rgb.rgb_of_values 0. 0. 0.) color_list in
  Rgb.normalize color_sum (float_of_int !num_points)
