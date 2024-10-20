open Geometry
open Domainslib

type camera = {
  forward: Geometry.Direction.t;
  left: Geometry.Direction.t;
  up: Geometry.Direction.t;
  origin: Geometry.Point.t;
  width: int;
  height: int;
}

let num_points = ref 8 

let camera up left forward origin (width, height) = {
  up; left; forward; origin;
  width; height;
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


let trace_ray scene ray : Colorspace.Rgb.pixel =
  match Figures.find_closest_figure scene ray with 
  | Some(fig) -> Figures.emission fig
  | None -> Colorspace.Rgb.rgb_of_values 0. 0. 0.

 
let pixel_color cam (row, col) scene pool =
  let open Colorspace in
  let pip_arr = BatArray.of_list (points_in_pixel cam (row, col)) in
  let color_sum () = Task.parallel_for_reduce ~start:0 ~finish:(BatArray.length pip_arr -1) ~body:(fun ind -> Direction.between_points pip_arr.(ind) cam.origin |> Figures.ray cam.origin |> trace_ray scene) pool
    (fun acc next_color -> Rgb.sum acc next_color) (Rgb.rgb_of_values 0. 0. 0.) in 
  Rgb.normalize (Task.run pool (fun _ -> color_sum ())) (float_of_int !num_points)
