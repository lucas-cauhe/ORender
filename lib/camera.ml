open Geometry
open Domainslib


let square (n : float) : float = n *. n

let kd = ref (Colorspace.Rgb.rgb_of_values 0.5 0.5 0.5)


type camera = {
  forward: Geometry.Direction.t;
  left: Geometry.Direction.t;
  up: Geometry.Direction.t;
  origin: Geometry.Point.t;
  width: int;
  height: int;
}

type light_source_type = {
  ls_center: Point.t;
  ls_power: Colorspace.Rgb.pixel
}

let num_points = ref 8

let camera up left forward origin (width, height) = {
  up; left; forward; origin;
  width; height;
}

let light_source c p = {
  ls_power = p;
  ls_center = c;
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


let trace_ray scene ray : Colorspace.Rgb.pixel * Figures.intersection_result =
  match Figures.find_closest_figure scene ray with 
  | Some(fig, ir) -> (Figures.emission (Figures.get_figure fig), ir)
  | None -> (Colorspace.Rgb.rgb_of_values 0. 0. 0., Zero)

let compute_color (scene : Figures.scene) (light_sources : light_source_type list) ((emission, ir): Colorspace.Rgb.pixel * Figures.intersection_result) : Colorspace.Rgb.pixel = 
  let open Colorspace in
  match ir with
  | Zero -> Colorspace.Rgb.rgb_of_values 0. 0. 0.
  | Intersects(intersection) -> begin
    let intersection = List.hd intersection in
    let color_of_ls ls = 
      let center_to_point = Direction.between_points ls.ls_center intersection.intersection_point in
      let ray_to_ls = Figures.ray intersection.intersection_point center_to_point in
      match Figures.find_closest_figure scene ray_to_ls with
      | Some(_, Intersects({distance = dist; _} :: _)) when dist < Direction.modulus center_to_point && dist > 10e-10 -> Colorspace.Rgb.rgb_of_values 0. 0. 0.
      | _ -> begin
        match Direction.normalize center_to_point with
        | None -> emission
        | Some(center_to_point_mod) -> 
          let li = Rgb.normalize ls.ls_power (Direction.modulus center_to_point |> square) in
          let brdf = Rgb.normalize !kd Float.pi in
          let rest = center_to_point_mod |> Direction.dot intersection.surface_normal |> abs_float in
          Rgb.value_prod (Rgb.rgb_prod li brdf) rest |> Rgb.rgb_prod emission
        end
    in
    List.fold_left (fun acc ls -> Rgb.sum acc (color_of_ls ls) ) (Rgb.rgb_of_values 0. 0. 0.) light_sources  
  end

let pixel_color cam (row, col) scene light_sources pool =
  let open Colorspace in
  let pip_arr = BatArray.of_list (points_in_pixel cam (row, col)) in
  let compute_pixel_color ind = Direction.between_points pip_arr.(ind) cam.origin |> Figures.ray cam.origin |> trace_ray scene |> compute_color scene light_sources in
  let color_sum () = Task.parallel_for_reduce ~start:0 ~finish:(BatArray.length pip_arr -1) ~body:compute_pixel_color pool
    (fun acc next_color -> Rgb.sum acc next_color) (Rgb.rgb_of_values 0. 0. 0.) in 
  Rgb.normalize (Task.run pool (fun _ -> color_sum ())) (float_of_int !num_points)
