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
  sphere_center : Geometry.Point.t; 
  sphere_radius : float 
}
type triangle_type = { 
  vert_a : Geometry.Point.t; 
  vert_b : Geometry.Point.t; 
  vert_c : Geometry.Point.t; 
  triangle_normal : Geometry.Direction.t 
}

type figure = Plane of plane_type | Sphere of sphere_type | Triangle of triangle_type
type scene = figure list

let square (n : float) : float = n *. n


let point_of_ray ray dist = 
  let open Geometry in
  let origin_dir = Direction.of_point ray.ray_origin in
  let dir_sum = Direction.prod ray.ray_direction dist |> Direction.sum origin_dir in
  Point.from_coords (Direction.x dir_sum) (Direction.y dir_sum) (Direction.z dir_sum)

(******************************)
(* PLANE ASSOCIATED FUNCTIONS *)
(******************************)

let plane d o = Plane({ plane_normal = d; plane_origin = o })

(* Plane implicit equation: ax + by + cz + d = 0 *)
(* Ray implicit equation: p + d * t = 0 *)
let plane_intersection (plane : plane_type) (ray : ray_type) : float list option =
  let open Geometry.Direction in
    (* Check if ray and plane are parallel *)
    match plane.plane_normal * ray.ray_direction with
    | 0. -> None
    | den ->
      let c = (of_point plane.plane_origin) * plane.plane_normal in
      let num = (of_point ray.ray_origin) * plane.plane_normal |> ( +. ) (-.c) in
      match -.num/.den with
      | neg when neg < 0. -> None
      | pos -> Some([pos])

let show_plane (plane : plane_type) = Printf.printf "Normal: %s, Origin: %s" (Geometry.Direction.string_of_direction plane.plane_normal) (Geometry.Point.string_of_point plane.plane_origin)

(*******************************)
(* SPHERE ASSOCIATED FUNCTIONS *)
(*******************************)

let sphere center radius = Sphere({ sphere_center = center; sphere_radius = radius })

let sphere_intersection (sphere : sphere_type ) (ray : ray_type) : float list option = 
  let module GPoint = Geometry.Point in
  let module GDirection = Geometry.Direction in
  let oc = GPoint.sub ray.ray_origin sphere.sphere_center in
  let a = GDirection.modulus ray.ray_direction |> square in
  let b = 2.0 *. (GDirection.dot (GDirection.of_point oc) ray.ray_direction) in
  let c = (GPoint.distance ray.ray_origin sphere.sphere_center |> square) -. (square sphere.sphere_radius) in
  let discriminant = (b *. b) -. (4.0 *. a *. c) in
  if discriminant < 0.0 then
    None
  else
    let t1 = (-.b -. sqrt discriminant) /. (2.0 *. a) in
    let t2 = (-.b +. sqrt discriminant) /. (2.0 *. a) in
    if t1 > t2 then Some([t2; t1]) else Some([t1; t2])

let show_sphere (sphere : sphere_type) = Printf.printf "Center: %s, Radius: %f" (Geometry.Point.string_of_point sphere.sphere_center) sphere.sphere_radius


(*********************************)
(* TRIANGLE ASSOCIATED FUNCTIONS *)
(*********************************)

let triangle a b c = 
  let (-) = Geometry.Point.sub in 
  match Geometry.Direction.cross_product (Geometry.Direction.of_point (b - a)) (Geometry.Direction.of_point (c - a)) |> Geometry.Direction.normalize with
  | Some(normal) -> Some(Triangle({vert_a = a; vert_b = b; vert_c = c;triangle_normal = normal}))
  | None -> None

let triangle_intersection (triangle : triangle_type) (ray : ray_type) = 
  let open Geometry.Direction in
    let (-) = Geometry.Point.sub in
    match plane_intersection {plane_normal = triangle.triangle_normal; plane_origin = triangle.vert_a} ray with
    | Some([d]) -> begin 
      let p = point_of_ray ray d in

      let v0 = triangle.vert_b - triangle.vert_a |> of_point in
      let v1 = triangle.vert_c - triangle.vert_b |> of_point in
      let v2 = triangle.vert_a - triangle.vert_c |> of_point in

      let pa = p - triangle.vert_a |> of_point in
      let pb = p - triangle.vert_b |> of_point in
      let pc = p - triangle.vert_c |> of_point in

      let left_a = cross_product v0 pa |> dot triangle.triangle_normal in
      let left_b = cross_product v1 pb |> dot triangle.triangle_normal in
      let left_c = cross_product v2 pc |> dot triangle.triangle_normal in

      if left_a >= 0. && left_b >= 0. && left_c >= 0. then Some([d]) else None
    end
    | _ -> None


let show_triangle (triangle: triangle_type) =  
  Printf.printf "A: %s, B: %s, C: %s, Normal: %s" (Geometry.Point.string_of_point triangle.vert_a) (Geometry.Point.string_of_point triangle.vert_b) (Geometry.Point.string_of_point triangle.vert_c) (Geometry.Direction.string_of_direction triangle.triangle_normal)

(*************************)
(* INTERSECTION FUNCTION *) 
(*************************)
  
let intersects fig ray = match fig with
| Plane(plane) -> plane_intersection plane ray
| Sphere(sphere) -> sphere_intersection sphere ray
| Triangle(triangle) -> triangle_intersection triangle ray

(*************************)
(*     SHOW FUNCTION     *) 
(*************************)

let show_figure fig = 
  match fig with
  | Plane(plane) -> show_plane plane
  | Sphere(sphere) -> show_sphere sphere
  | Triangle(triangle) -> show_triangle triangle 

(** Returns [None] if ray doesn't intersect any figure in the scene. Otherwise return the first figure in the scene that the ray intersects with
  wrapped in [Some] 
  As it is implemented, thread safety is guaranteed since intersections are stored in an array at their figure's scene position
  Rather than workin with locks for computing the minimum on the fly we've decided to perform a min operation over the intersections array later.
*)
let closest_figure pool (scene : figure BatArray.t ) (intersections : float option BatArray.t) (ray : ray_type) : (figure * float) option = 
  Task.parallel_for pool ~start:0 ~finish:(BatArray.length scene - 1) ~body:(fun i ->
    match intersects scene.(i) ray with
    | Some(closest :: _) -> intersections.(i) <- Some(closest)
    | _ -> intersections.(i) <- None
  );
  let min (min_set : (figure * float) option) ind next = match min_set, next with
  | None, None -> None
  | None, Some(curr) -> Some((scene.(ind), curr))
  | Some(_), None -> min_set
  | Some((_, dist_min)), Some(curr) -> if dist_min < curr then min_set else Some((scene.(ind), curr) ) in
  let result = BatArray.fold_lefti min None intersections in
  result

let find_closest_figure s ray = 
  let pool = Task.setup_pool ~num_domains:7 () in
  let scene_arr = BatArray.of_list s in
  let intersections = BatArray.init (List.length s) (fun _ -> None) in
  let fig = Task.run pool (fun () -> closest_figure pool scene_arr intersections ray) in
  Task.teardown_pool pool;
  match fig with
  | Some(f, _) -> Some(f)
  | _ -> None

