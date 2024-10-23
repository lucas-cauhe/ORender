(*
  Authors: Lucas Cauhé Viñao, Andrei Dumbrava
  Description: Figures interface implementation
*)

open Geometry

type ray_type = {
  ray_origin: Point.t;
  ray_direction: Direction.t
}

let ray o d = {ray_origin = o; ray_direction = d}
let string_of_ray ray = Printf.sprintf "Ray dir -> %s, Ray origin -> %s" (Direction.string_of_direction ray.ray_direction) (Point.string_of_point ray.ray_origin)

type plane_type = {
  plane_normal: Direction.t;
  plane_origin: Point.t;
}

type sphere_type = { 
  sphere_radius : float;
  sphere_center : Point.t;
}
type triangle_type = { 
  vert_a : Point.t; 
  vert_b : Point.t; 
  vert_c : Point.t; 
  triangle_normal : Direction.t 
}

type cuboid_type = {
  cuboid_min : Point.t;
  cuboid_max : Point.t;
}

(* type cylinder_type = {
  cylinder_radius : float;
  cylinder_base_center : Point.t;
  cylinder_axis : Direction.t; (* |cylinder_axis| = cylinder's height *)
} *)

type figure_type = Empty 
  | Plane of plane_type 
  | Sphere of sphere_type 
  | Triangle of triangle_type
  | Cuboid of cuboid_type
  (* | Cylinder of cylinder_type *)
type figure = { fig_type: figure_type; emission: Colorspace.Rgb.pixel }
type scene_figure = Figure of figure | BoundingBox of figure * scene_figure list
type scene = scene_figure list

type intersection = {
  distance: float;
  surface_normal: Direction.t; 
  intersection_point: Point.t;
}
type intersection_result = Zero | Intersects of intersection list

let square (n : float) : float = n *. n

let get_figure = function Figure(fig) -> fig
  | BoundingBox(fig, _) -> fig

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
  let open Direction in
  (* Check if ray and plane are parallel *)
  match plane.plane_normal * ray.ray_direction with
  | 0. -> Zero
  | den ->
    let c = (of_point plane.plane_origin) * plane.plane_normal in
    let num = (of_point ray.ray_origin) * plane.plane_normal |> ( +. ) (-.c) in
    match -.num/.den with
    | neg when neg <= 10e-10 -> Zero
    | pos -> Intersects([{distance = pos; surface_normal = plane.plane_normal; intersection_point = point_of_ray ray pos}])

let show_plane (plane : plane_type) = Printf.printf "PLANE {Normal: %s, Origin: %s}\n" (Direction.string_of_direction plane.plane_normal) (Point.string_of_point plane.plane_origin)

let transform_plane _ fig e = Some(plane fig.plane_normal fig.plane_origin e)

(*******************************)
(* SPHERE ASSOCIATED FUNCTIONS *)
(*******************************)

let sphere center radius e = {fig_type = Sphere({ sphere_center = center; sphere_radius = radius }); emission = e}

let sphere_intersection (sphere : sphere_type ) (ray : ray_type) : intersection_result = 
  let module GPoint = Point in
  let module GDirection = Direction in
  let surface_normal d = GDirection.between_points (point_of_ray ray d) sphere.sphere_center in

  let oc = GDirection.between_points ray.ray_origin sphere.sphere_center in
  let a = GDirection.modulus ray.ray_direction |> square in
  let b = 2.0 *. (GDirection.dot oc ray.ray_direction) in
  let c = (GPoint.distance ray.ray_origin sphere.sphere_center |> square) -. (square sphere.sphere_radius) in

  let discriminant = (b *. b) -. (4.0 *. a *. c) in
  if abs_float discriminant <= 1e-10 then
    let dist = -.b /. (2. *. a) in 
    Intersects([{distance = dist; surface_normal = surface_normal dist; intersection_point = point_of_ray ray dist }])
  else
    if discriminant > 0. then 
      let t1 = (-.b -. sqrt discriminant) /. (2.0 *. a) in
      let t2 = (-.b +. sqrt discriminant) /. (2.0 *. a) in
      let fst = if t1 > 10e-10 then [{ distance = t1; surface_normal = surface_normal t1; intersection_point = point_of_ray ray t1 }] else [] in
      let snd =  if t2 > 0. then [{ distance = t2; surface_normal = surface_normal t2; intersection_point = point_of_ray ray t2 }] else [] in
      match fst, snd with
      | [], [] -> Zero
      | f, s -> Intersects(f @ s)
    else
      Zero
    

let show_sphere (sphere : sphere_type) = Printf.printf "SPHERE {Center: %s, Radius: %f}\n" (Point.string_of_point sphere.sphere_center) sphere.sphere_radius

let transform_sphere _ fig e = Some(sphere fig.sphere_center fig.sphere_radius e)

(*********************************)
(* TRIANGLE ASSOCIATED FUNCTIONS *)
(*********************************)

let triangle a b c e = 
  match Direction.cross_product (Direction.between_points b a) (Direction.between_points c a) |> Direction.normalize with
  | Some(normal) -> Some({fig_type = Triangle({vert_a = a; vert_b = b; vert_c = c;triangle_normal = normal}); emission = e})
  | None -> None

let triangle_intersection (triangle : triangle_type) (ray : ray_type) = 
  let open Direction in
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
  Printf.printf "TRIANGLE {A: %s, B: %s, C: %s, Normal: %s}\n" (Point.string_of_point triangle.vert_a) (Point.string_of_point triangle.vert_b) (Point.string_of_point triangle.vert_c) (Direction.string_of_direction triangle.triangle_normal)


let transform_triangle (transform : transformation) (t : triangle_type) (e : Colorspace.Rgb.pixel) : figure option = 
  match transform with
  | Translation(tx, ty, tz) -> 
    let translation_mat = Transformations.translation_transformation_of_values tx ty tz in
    let translated_points = Array.map 
      (fun point -> Transformations.hc_of_point point |> Transformations.translate translation_mat |> Option.get |> Transformations.point_of_hc)
      [|t.vert_a; t.vert_b; t.vert_c|] in
    triangle translated_points.(0) translated_points.(1) translated_points.(2) e
  | Scale(sx, sy, sz) -> 
    let scale_mat = Transformations.scale_transformation_of_values sx sy sz in
    let scaled_dirs = Array.map
      (fun dir -> Transformations.hc_of_direction dir |> Transformations.scale scale_mat |> Transformations.direction_of_hc)
      [| Direction.between_points t.vert_c t.vert_a; Direction.between_points t.vert_b t.vert_a |] in
    triangle 
      t.vert_a 
      (Point.sum t.vert_a (Point.from_coords (Direction.x scaled_dirs.(0)) (Direction.y scaled_dirs.(0)) (Direction.z scaled_dirs.(0)) )) 
      (Point.sum t.vert_a (Point.from_coords (Direction.x scaled_dirs.(1)) (Direction.y scaled_dirs.(1)) (Direction.z scaled_dirs.(1)) )) 
      e
  | Rotation(m, ax) ->
    let rotated_points = Array.map
      (fun point -> Transformations.hc_of_point point |> Transformations.rotate m ax |>  Transformations.point_of_hc)
      [| t.vert_a; t.vert_b; t.vert_c |] in
    triangle rotated_points.(0) rotated_points.(1) rotated_points.(2) e
  | _ -> triangle t.vert_a t.vert_b t.vert_c e


(*******************************)
(* CUBOID ASSOCIATED FUNCTIONS *) 
(*******************************)

let cuboid c_min c_max e = {fig_type = Cuboid({ cuboid_min = c_min; cuboid_max = c_max; }); emission = e; }

let cuboid_intersection (c : cuboid_type) (ray : ray_type) : intersection_result =
  let t1x = (Point.x c.cuboid_min -. Point.x ray.ray_origin) /. Direction.x ray.ray_direction in
  let t2x = (Point.x c.cuboid_max -. Point.x ray.ray_origin) /. Direction.x ray.ray_direction in
  let t1y = (Point.y c.cuboid_min -. Point.y ray.ray_origin) /. Direction.y ray.ray_direction in
  let t2y = (Point.y c.cuboid_max -. Point.y ray.ray_origin) /. Direction.y ray.ray_direction in
  let t1z = (Point.z c.cuboid_min -. Point.z ray.ray_origin) /. Direction.z ray.ray_direction in
  let t2z = (Point.z c.cuboid_max -. Point.z ray.ray_origin) /. Direction.z ray.ray_direction in

  let tmin = max (min t1x t2x) (min t1y t2y) |> max (min t1z t2z) in
  let tmax = min (max t1x t2x) (max t1y t2y) |> min (max t1z t2z) in

  if tmin < tmax && tmax > 0. then
    let distance = if tmin > 0. then tmin else tmax in
    let normal = 
      if distance = t1x || distance = t2x then Direction.from_coords (if distance = t1x then -1. else 1.) 0. 0.
      else if distance = t1y || distance = t2y then Direction.from_coords 0. (if distance = t1y then -1. else 1.) 0.
      else Direction.from_coords 0. 0. (if distance = t1z then -1. else 1.)
    in
    Intersects([{distance; surface_normal = normal; intersection_point = point_of_ray ray distance}])
  else
    Zero
  

let show_cuboid (cuboid : cuboid_type) = 
  Printf.printf "min = %s; max = %s" (Point.string_of_point cuboid.cuboid_min) (Point.string_of_point cuboid.cuboid_max)


(*************************)
(* INTERSECTION FUNCTION *) 
(*************************)
  
let intersects fig ray = match fig.fig_type with
| Plane(plane) -> plane_intersection plane ray
| Sphere(sphere) -> sphere_intersection sphere ray
| Triangle(triangle) -> triangle_intersection triangle ray
| Cuboid(cuboid) -> cuboid_intersection cuboid ray
| Empty -> Zero

(*************************)
(*     SHOW FUNCTION     *) 
(*************************)

let show_figure fig = 
  match fig.fig_type with
  | Plane(plane) -> show_plane plane
  | Sphere(sphere) -> show_sphere sphere
  | Triangle(triangle) -> show_triangle triangle 
  | Cuboid(cuboid) -> show_cuboid cuboid
  | Empty -> print_endline "Empty figure"

(*****************************)
(*    TRANSFORM FUNCTION     *) 
(*****************************)
let transform t fig = 
  match fig.fig_type with 
  | Empty -> None  
  | Plane(plane) -> transform_plane t plane fig.emission
  | Sphere(sphere) -> transform_sphere t sphere fig.emission
  | Triangle(triangle) -> transform_triangle t triangle fig.emission 
  | Cuboid(_) -> Some(fig) 

let _ = Empty



(** Returns [None] if ray doesn't intersect any figure in the scene. Otherwise return the first figure in the scene that the ray intersects with
  wrapped in [Some] 
  As it is implemented, thread safety is guaranteed since intersections are stored in an array at their figure's scene position
  Rather than workin with locks for computing the minimum on the fly we've decided to perform a min operation over the intersections array later.
*)
let rec find_closest_figure scene ray = 
  let min (min_set : scene_figure * intersection_result) fig next = match min_set, next with
  | _, Zero -> min_set
  | (_, Zero), Intersects(_) -> (fig, next)
  | (_, Intersects(dist_min)), Intersects(curr) -> if (List.hd dist_min).distance < (List.hd curr).distance then min_set else (fig, next) in
  let rec loop_ closest figures = 
    match figures with
    | [] -> closest
    | fig :: rest -> begin
      match fig with
      | Figure(f) -> loop_ (min closest fig (intersects f ray)) rest 
      | BoundingBox(f, rest_figures) -> 
        match intersects f ray with
        | Zero -> loop_ closest rest 
        | Intersects(_) -> 
          match find_closest_figure rest_figures ray with 
          | None -> loop_ closest rest
          | Some(inner_figure, _) -> loop_ (min closest inner_figure (intersects (get_figure inner_figure) ray)) rest
        
      end in
  match loop_ (List.hd scene, Zero) scene with
  | _, Zero -> None
  | fig, (Intersects(_) as inter) -> Some((fig, inter)) 


