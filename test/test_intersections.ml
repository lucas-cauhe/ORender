open Alcotest
open Computer_gfx.Figures
open Computer_gfx.Geometry


let testable_intersection = 
  let module M = struct
    type t = float list option
    let pp fmt = function None -> Format.fprintf fmt "No intersection\n"
    | Some(il) -> List.iter (fun f -> Format.fprintf fmt "%f | " f) il; print_newline ()
    let equal i1 i2 = match i1, i2 with
    | None, Some(_) -> false
    | Some(_), None -> false
    | None, None -> true
    | Some(l1), Some(l2) -> List.length l1 = List.length l2
  end in
  (module M : Alcotest.TESTABLE with type t = M.t)

(****************)
(* SPHERE TESTS *)
(****************)

let test_sphere_one_intersection _ =
  (* Defines una esfera cualquiera *)
  let center = Point.from_coords 10. 10. 10. in
  let r = ref 10. in
  let sphere = sphere center !r in
  (* El punto donde van a interseccionar el rayo y la esfera lo defines *)
  (* mediante la latitud y el acimut *)
  let lat = ref 60. in
  let acimut = ref 30. in
  let k = ref 5. in (* cte para tomar el origen *)
  let surface_p = Point.from_coords (!r *. sin !lat *. cos !acimut) (!r *. sin !lat *. sin !acimut) (!r *. cos !lat) in
  
  (* Calculas uno de los vectores tangeciales (de cambio de base) a la superficie de la esfera *)
  (* En ese punto y usas ese para trazar el rayo *)
  (* En este caso se ha escogido el vector tangente al acimut, manteniendo la latitud constante *)
  let ray_dir = Direction.from_coords (-.(!r) *. cos !lat *. sin !acimut) (!r *. cos !lat *. cos !acimut) 0. in
  let p = point_of_ray { ray_origin = surface_p; ray_direction = ray_dir } (-.(!k)) in
  let ray = { ray_origin = p; ray_direction = ray_dir } in
  
  check testable_intersection "Intersections differ from 1" (Some [1.]) (intersects sphere ray)

(***************)
(* PLANE TESTS *)
(***************)

let test_plane_parallel _ =
  let ray_dir = Direction.from_coords 1. 1. 1. in
  let ray = { ray_origin = (Point.from_coords 0. 0. 0.); ray_direction = ray_dir } in
  let plane_normal = Direction.from_coords (-1.) 0.5 0.5 in
  if Direction.dot plane_normal ray_dir <> 0. then failwith "not perpendicular";
  (* Point must be present in both plane and ray's direction *)
  let perp_plane = plane plane_normal (Point.from_coords 1. 1. 1.) in
  check testable_intersection "Ray & Plane intersect (not orthogonal)" None (intersects perp_plane ray)

let test_plane_intersects _ = 
  let ray = { ray_origin = (Point.from_coords 0. 0. 0.); ray_direction = (Direction.from_coords 1. 1. 1.) } in
  let plane = plane (Direction.from_coords (-1.) 0. 0.) (Point.from_coords 3. 2. 1.) in
  check testable_intersection "Ray & Plane don't intersect" (Some[1.]) (intersects plane ray)

let test_plane_not_intersects _ = 
  let ray = { ray_origin = (Point.from_coords 0. 0. 0.); ray_direction = (Direction.from_coords 1. 1. 1.) } in
  let plane = plane (Direction.from_coords (-1.) 0. 0.) (Point.from_coords (-3.) 2. 1.) in
  check testable_intersection "Ray & Plane intersect" None (intersects plane ray)


(******************)
(* TRIANGLE TESTS *)
(******************)

(* Ray passes through the triangle *)
let test_triangle_intersects _ = 
  let ray = { ray_origin = (Point.from_coords 0. 0. 0.); ray_direction = (Direction.from_coords 1. 1. 1.) } in
  let triangle = triangle (Point.from_coords 2. 0. 2.) (Point.from_coords 2. 2. 0.) (Point.from_coords 0. 2. 2.) in
  match triangle with
  | None -> failwith "bad triangle definition"
  | Some(t) -> check testable_intersection "Ray & Triangle dont intersect" (Some[1.]) (intersects t ray)

(* Ray doesn't pass through the triangle *)
let test_triangle_not_intersects _ = 
  let ray = { ray_origin = (Point.from_coords 0. 0. 0.); ray_direction = (Direction.from_coords 1. 1. 1.) } in
  let triangle = triangle (Point.from_coords 2. 0. 1.) (Point.from_coords 2. 2. 1.) (Point.from_coords 4. 0. 1.) in
  match triangle with
  | None -> failwith "bad triangle definition"
  | Some(t) -> check testable_intersection "Ray & Triangle intersect" None (intersects t ray)
  



let test_sphere = [
  "test_sphere_one_intersection", `Quick, test_sphere_one_intersection
]

let test_plane = [
  "test_plane_parallel", `Quick, test_plane_parallel;
  "test_plane_intersects", `Quick, test_plane_intersects;
  "test_plane_not_intersects", `Quick, test_plane_not_intersects;
]

let test_triangle = [
  "test_triangle_intersects", `Quick, test_triangle_intersects;
  "test_triangle_not_intersects", `Quick, test_triangle_not_intersects;
]

let () = Alcotest.run "Figures" [
  "Plane", test_plane;
  "Triangle", test_triangle;
  "Sphere", test_sphere;
  
]