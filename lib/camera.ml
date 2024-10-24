open Geometry
open Colorspace
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
  | Some(fig, ir) -> begin
    match Figures.is_sphere fig, ir with
    | true, Intersects(intersection :: _) -> 
      let surface_normal_ray = Figures.ray intersection.intersection_point intersection.surface_normal in
      let moved_ip = Figures.point_of_ray surface_normal_ray 0.001 in
      (Figures.emission (Figures.get_figure fig), Intersects([{ intersection with intersection_point = moved_ip}]))
    | _ -> (Figures.emission (Figures.get_figure fig), ir)
  end
  | None -> (Colorspace.Rgb.rgb_of_values 0. 0. 0., Zero)

let color_of_ls (scene : Figures.scene) (ls : light_source_type) (ir : Figures.intersection) (emission : Rgb.pixel) : Colorspace.Rgb.pixel = 
  match  Direction.between_points ls.ls_center ir.intersection_point |> Direction.normalize with
  | None -> emission
  | Some(center_to_point) -> begin 
    let ray_to_ls = Figures.ray ir.intersection_point center_to_point in
    match Figures.find_closest_figure scene ray_to_ls with
    | Some(_, Intersects({distance = dist; _} :: _)) when dist < Figures.dist_to_point_of_ray ray_to_ls ls.ls_center && dist > 10e-5 -> (*Printf.printf "%s vs %s for distances %f vs %f\n" (Figures.string_of_ray ray_to_ls) (Point.string_of_point ip) (Direction.modulus center_to_point) dist;*) Colorspace.Rgb.rgb_of_values 0. 0. 0.
    | Some(_, Intersects({distance = _dist; intersection_point = _ip; _} :: _)) -> 
      (* Printf.printf "%s vs %s for distances %f vs %f\n" (Figures.string_of_ray ray_to_ls) (Point.string_of_point ip) (Direction.modulus center_to_point) dist;  *)
      let li = Rgb.normalize ls.ls_power (Direction.modulus center_to_point |> square) in
      let brdf = Rgb.normalize !kd Float.pi in
      let rest = center_to_point |> Direction.dot ir.surface_normal |> abs_float in
      Rgb.value_prod (Rgb.rgb_prod li brdf) rest |> Rgb.rgb_prod emission
    | _ -> emission
  end


let compute_color (scene : Figures.scene) (light_sources : light_source_type list) ((emission, ir): Colorspace.Rgb.pixel * Figures.intersection_result) : Colorspace.Rgb.pixel = 
  match ir with
  | Zero -> Colorspace.Rgb.rgb_of_values 0. 0. 0.
  | Intersects(intersection) -> begin
    let intersection = List.hd intersection in
    List.fold_left (fun acc ls -> Rgb.sum acc (color_of_ls scene ls intersection emission) ) (Rgb.rgb_of_values 0. 0. 0.) light_sources  
  end

let pixel_color cam (row, col) scene light_sources pool =
  let open Colorspace in
  let pip_arr = BatArray.of_list (points_in_pixel cam (row, col)) in
  let compute_pixel_color ind = Direction.between_points pip_arr.(ind) cam.origin |> Figures.ray cam.origin |> trace_ray scene |> compute_color scene light_sources in
  let color_sum () = Task.parallel_for_reduce ~start:0 ~finish:(BatArray.length pip_arr -1) ~body:compute_pixel_color pool
    (fun acc next_color -> Rgb.sum acc next_color) (Rgb.rgb_of_values 0. 0. 0.) in 
  Rgb.normalize (Task.run pool (fun _ -> color_sum ())) (float_of_int !num_points)
