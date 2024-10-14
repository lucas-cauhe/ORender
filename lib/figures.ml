(*
  Authors: Lucas Cauhé Viñao, Andrei Dumbrava
  Description: Figures interface implementation
*)

open Domainslib

type ray_type = {
  ray_origin: Geometry.Point.t;
  ray_direction: Geometry.Direction.t
}

type plane_type = {
  plane_normal: Geometry.Direction.t;
  plane_origin: Geometry.Point.t;
}

type sphere_type = { 
  sphere_radius : float;
  sphere_center : Geometry.Point.t;
}
type triangle_type = { 
  vert_a : Geometry.Point.t; 
  vert_b : Geometry.Point.t; 
  vert_c : Geometry.Point.t; 
  triangle_normal : Geometry.Direction.t 
}

(* type cylinder_type = {
  cylinder_radius : float;
  cylinder_base_center : Geometry.Point.t;
  cylinder_axis : Geometry.Direction.t; (* |cylinder_axis| = cylinder's height *)
} *)

type figure_type = Empty 
  | Plane of plane_type 
  | Sphere of sphere_type 
  | Triangle of triangle_type
  (* | Cylinder of cylinder_type *)
type figure = { fig_type: figure_type; emission: Colorspace.Rgb.pixel }
type scene = figure list

type intersection = {
  distance: float;
  surface_normal: Geometry.Direction.t
}
type intersection_result = Zero | Intersects of intersection list

let square (n : float) : float = n *. n


let point_of_ray ray dist = 
  let open Geometry in
  let origin_dir = Direction.of_point ray.ray_origin in
  let dir_sum = Direction.prod ray.ray_direction dist |> Direction.sum origin_dir in
  Point.from_coords (Direction.x dir_sum) (Direction.y dir_sum) (Direction.z dir_sum)

let emission fig = fig.emission

(******************************)
(* PLANE ASSOCIATED FUNCTIONS *)
(******************************)

let plane d o e = { fig_type = Plane({ plane_normal = d; plane_origin = o }); emission = e }

(* Plane implicit equation: ax + by + cz + d = 0 *)
(* Ray implicit equation: p + d * t = 0 *)
let plane_intersection (plane : plane_type) (ray : ray_type) : intersection_result =
  let open Geometry.Direction in
  (* Check if ray and plane are parallel *)
  match plane.plane_normal * ray.ray_direction with
  | 0. -> Zero
  | den ->
    let c = (of_point plane.plane_origin) * plane.plane_normal in
    let num = (of_point ray.ray_origin) * plane.plane_normal |> ( +. ) (-.c) in
    match -.num/.den with
    | neg when neg < 0. -> Zero
    | pos -> Intersects([{distance = pos; surface_normal = plane.plane_normal}])

let show_plane (plane : plane_type) = Printf.printf "Normal: %s, Origin: %s" (Geometry.Direction.string_of_direction plane.plane_normal) (Geometry.Point.string_of_point plane.plane_origin)

(*******************************)
(* SPHERE ASSOCIATED FUNCTIONS *)
(*******************************)

let sphere center radius e = {fig_type = Sphere({ sphere_center = center; sphere_radius = radius }); emission = e}

let sphere_intersection (sphere : sphere_type ) (ray : ray_type) : intersection_result = 
  let module GPoint = Geometry.Point in
  let module GDirection = Geometry.Direction in
  let surface_normal d = GDirection.between_points (point_of_ray ray d) sphere.sphere_center in

  let oc = GDirection.between_points ray.ray_origin sphere.sphere_center in
  let a = GDirection.modulus ray.ray_direction |> square in
  let b = 2.0 *. (GDirection.dot oc ray.ray_direction) in
  let c = (GPoint.distance ray.ray_origin sphere.sphere_center |> square) -. (square sphere.sphere_radius) in

  let discriminant = (b *. b) -. (4.0 *. a *. c) in
  if abs_float discriminant <= 1e-10 then
    let dist = -.b /. (2. *. a) in 
    Intersects([{distance = dist; surface_normal = surface_normal dist }])
  else
    if discriminant > 0. then 
      let t1 = (-.b -. sqrt discriminant) /. (2.0 *. a) in
      let t2 = (-.b +. sqrt discriminant) /. (2.0 *. a) in
      let fst = if t1 > 0. then [{ distance = t1; surface_normal = surface_normal t1 }] else [] in
      let snd =  if t2 > 0. then [{ distance = t2; surface_normal = surface_normal t2 }] else [] in
      match fst, snd with
      | [], [] -> Zero
      | f, s -> Intersects(f @ s)
    else
      Zero
    

let show_sphere (sphere : sphere_type) = Printf.printf "Center: %s, Radius: %f" (Geometry.Point.string_of_point sphere.sphere_center) sphere.sphere_radius


(*********************************)
(* TRIANGLE ASSOCIATED FUNCTIONS *)
(*********************************)

let triangle a b c e = 
  match Geometry.Direction.cross_product (Geometry.Direction.between_points b a) (Geometry.Direction.between_points c a) |> Geometry.Direction.normalize with
  | Some(normal) -> Some({fig_type = Triangle({vert_a = a; vert_b = b; vert_c = c;triangle_normal = normal}); emission = e})
  | None -> None

let triangle_intersection (triangle : triangle_type) (ray : ray_type) = 
  let open Geometry.Direction in
    match plane_intersection {plane_normal = triangle.triangle_normal; plane_origin = triangle.vert_a} ray with
    | Intersects([{ distance = d; _ }]) as il -> begin 
      let p = point_of_ray ray d in

      let v0 = between_points triangle.vert_b triangle.vert_a in
      let v1 = between_points triangle.vert_c triangle.vert_b in
      let v2 = between_points triangle.vert_a triangle.vert_c in

      let pa = between_points p triangle.vert_a in
      let pb = between_points p triangle.vert_b in
      let pc = between_points p triangle.vert_c in

      let left_a = cross_product v0 pa |> dot triangle.triangle_normal in
      let left_b = cross_product v1 pb |> dot triangle.triangle_normal in
      let left_c = cross_product v2 pc |> dot triangle.triangle_normal in

      if left_a >= 0. && left_b >= 0. && left_c >= 0. then il else Zero
    end
    | _ -> Zero


let show_triangle (triangle: triangle_type) =  
  Printf.printf "A: %s, B: %s, C: %s, Normal: %s" (Geometry.Point.string_of_point triangle.vert_a) (Geometry.Point.string_of_point triangle.vert_b) (Geometry.Point.string_of_point triangle.vert_c) (Geometry.Direction.string_of_direction triangle.triangle_normal)

(*************************)
(* INTERSECTION FUNCTION *) 
(*************************)
  
let intersects fig ray = match fig.fig_type with
| Plane(plane) -> plane_intersection plane ray
| Sphere(sphere) -> sphere_intersection sphere ray
| Triangle(triangle) -> triangle_intersection triangle ray
| Empty -> Zero

(*************************)
(*     SHOW FUNCTION     *) 
(*************************)

let show_figure fig = 
  match fig.fig_type with
  | Plane(plane) -> show_plane plane
  | Sphere(sphere) -> show_sphere sphere
  | Triangle(triangle) -> show_triangle triangle 
  | Empty -> print_endline "Empty figure"

(** Returns [None] if ray doesn't intersect any figure in the scene. Otherwise return the first figure in the scene that the ray intersects with
  wrapped in [Some] 
  As it is implemented, thread safety is guaranteed since intersections are stored in an array at their figure's scene position
  Rather than workin with locks for computing the minimum on the fly we've decided to perform a min operation over the intersections array later.
*)
let closest_figure pool (scene : figure BatArray.t ) (intersections : intersection_result BatArray.t) (ray : ray_type) : (figure * intersection_result) option = 
  Task.parallel_for pool ~start:0 ~finish:(BatArray.length scene - 1) ~body:(fun i ->
    match intersects scene.(i) ray with
    | Intersects(closest :: _) -> intersections.(i) <- Intersects([closest])
    | _ -> intersections.(i) <- Zero
  );
  let min (min_set : figure * intersection_result) ind next = match min_set, next with
  | _, Zero -> min_set
  | (_, Zero), Intersects(_) -> (scene.(ind), next)
  | (_, Intersects(dist_min)), Intersects(curr) -> if (List.hd dist_min).distance < (List.hd curr).distance then min_set else (scene.(ind), next) in
  match BatArray.fold_lefti min ({ fig_type = Empty; emission = Colorspace.Rgb.rgb_of_values 0. 0. 0. }, Zero) intersections with
  | (_, Zero) -> None
  | (_, Intersects(_)) as result -> Some(result)

let find_closest_figure s ray = 
  let pool = Task.setup_pool ~num_domains:7 () in
  let scene_arr = BatArray.of_list s in
  let intersections = BatArray.init (List.length s) (fun _ -> Zero) in
  let fig = Task.run pool (fun () -> closest_figure pool scene_arr intersections ray) in
  Task.teardown_pool pool;
  match fig with
  | Some(f, _) -> Some(f)
  | _ -> None

