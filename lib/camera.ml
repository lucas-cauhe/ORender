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

let point_in_pixel cam (row, col) =
  let open Geometry in
  let left_x = Direction.x cam.left *. (1. -. float_of_int col /. (float_of_int cam.width /. 2.)) in
  let left_y = Direction.y cam.left *. (1. -. float_of_int col /. (float_of_int cam.height /. 2.)) in
  let up_x = Direction.x cam.up *. (1. -. float_of_int row /. (float_of_int cam.height /. 2.)) in
  let up_y = Direction.y cam.up *. (1. -. float_of_int row /. (float_of_int cam.height /. 2.)) in
  let x = left_x +. up_x in
  let y = left_y +. up_y in
  Geometry.Point.from_coords x y ((Point.z cam.origin) +. (Direction.z cam.forward))

 
let trace_ray cam (row, col) scene pool =
  let ray_direction = Geometry.Direction.between_points (point_in_pixel cam (row, col)) cam.origin in
  let ray = Figures.ray cam.origin ray_direction in
  match Figures.find_closest_figure scene ray pool with 
  | Some(fig) -> Figures.emission fig
  | None -> Colorspace.Rgb.rgb_of_values 0. 0. 0.
