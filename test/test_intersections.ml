open Alcotest
open Computer_gfx.Figures
open Computer_gfx.Geometry
open Computer_gfx.Colorspace

let color = ref (Rgb.rgb_of_values 255. 0. 0.)
let one_intersection = ref (Intersects [{distance = 1.; surface_normal = Direction.from_coords 1. 1. 1.}])
let two_intersection = ref (Intersects [
  {distance = 1.; surface_normal = Direction.from_coords 1. 1. 1.};
  {distance = 1.; surface_normal = Direction.from_coords 1. 1. 1.}
])


let testable_intersection = 
  let module M = struct
    type t = intersection_result
    let pp fmt = function Zero -> Format.fprintf fmt "No intersection\n"
    | Intersects(il) -> List.iter (fun f -> Format.fprintf fmt "{ dist = %f; snormal = %s } | " f.distance (Direction.string_of_direction f.surface_normal)) il; print_newline ()
    let equal i1 i2 = match i1, i2 with
    | Zero, Intersects(_) -> false
    | Intersects(_), Zero -> false
    | Zero, Zero -> true
    | Intersects(l1), Intersects(l2) -> List.length l1 = List.length l2
  end in
  (module M : Alcotest.TESTABLE with type t = M.t)

(****************)
(* SPHERE TESTS *)
(****************)

let test_sphere_no_intersection _ = 
  let sphere = sphere (Point.from_coords 4. 0. 0.) 1. !color in
  let ray = { ray_origin = Point.from_coords 0. 0. 0.; ray_direction = Direction.from_coords 1. 1. 1. } in
  check testable_intersection "Found intersection" Zero (intersects sphere ray)

let test_sphere_one_intersection _ =
  (* Defines una esfera cualquiera *)
  let center = Point.from_coords 10. 10. 10. in
  let r = ref 10. in
  let sphere = sphere center !r !color in
  (* El punto donde van a interseccionar el rayo y la esfera lo defines *)
  (* mediante la latitud y el acimut *)
  let lat = ref (Float.pi/.2.) in
  let acimut = ref (Float.pi/.2.) in
  let k = ref 5. in (* cte para tomar el origen *)
  let w = Direction.from_coords (!r *. sin !lat *. cos !acimut) (!r *. sin !lat *. sin !acimut) (!r *. cos !lat) in
  let surface_p = Point.from_coords (Direction.x w) (Direction.y w) (Direction.z w) in
  
  (* Calculas uno de los vectores tangeciales (de cambio de base) a la superficie de la esfera *)
  (* En ese punto y usas ese para trazar el rayo *)
  (* En este caso se ha escogido el vector tangente al acimut, manteniendo la latitud constante *)
  let ray_dir = Direction.from_coords (-.(!r) *. sin !lat *. sin !acimut) (!r *. sin !lat *. cos !acimut) 0. in
  if Direction.dot ray_dir w <> 0. then failwith "not perpendicular";
  let p = point_of_ray { ray_origin = surface_p; ray_direction = ray_dir } (-.(!k)) in
  let ray = { ray_origin = p; ray_direction = ray_dir } in
  
  check testable_intersection "Intersections differ from 1" !one_intersection (intersects sphere ray)

let test_sphere_two_intersection _ = 
  let sphere = sphere (Point.from_coords 4. 3. 5.) 4. !color in
  let ray = { ray_origin = Point.from_coords 0. 0. 0.; ray_direction = Direction.from_coords 1. 1. 1. } in
  check testable_intersection "Found intersection" !two_intersection (intersects sphere ray)

(* Here the ray starts from within the sphere *)
let test_sphere_one_intersection_from_origin _ = 
  let sphere = sphere (Point.from_coords 4. 3. 5.) 10. !color in
  let ray = { ray_origin = Point.from_coords 0. 0. 0.; ray_direction = Direction.from_coords 1. 1. 1. } in
  check testable_intersection "Found intersection" !one_intersection (intersects sphere ray)

(***************)
(* PLANE TESTS *)
(***************)

let test_plane_parallel _ =
  let ray_dir = Direction.from_coords 1. 1. 1. in
  let ray = { ray_origin = (Point.from_coords 0. 0. 0.); ray_direction = ray_dir } in
  let plane_normal = Direction.from_coords (-1.) 0.5 0.5 in
  if Direction.dot plane_normal ray_dir <> 0. then failwith "not perpendicular";
  (* Point must be present in both plane and ray's direction *)
  let perp_plane = plane plane_normal (Point.from_coords 1. 1. 1.) !color in
  check testable_intersection "Ray & Plane intersect (not orthogonal)" Zero (intersects perp_plane ray)

let test_plane_intersects _ = 
  let ray = { ray_origin = (Point.from_coords 0. 0. 0.); ray_direction = (Direction.from_coords 1. 1. 1.) } in
  let plane = plane (Direction.from_coords (-1.) 0. 0.) (Point.from_coords 3. 2. 1.) !color in
  check testable_intersection "Ray & Plane don't intersect" !one_intersection (intersects plane ray)

let test_plane_not_intersects _ = 
  let ray = { ray_origin = (Point.from_coords 0. 0. 0.); ray_direction = (Direction.from_coords 1. 1. 1.) } in
  let plane = plane (Direction.from_coords (-1.) 0. 0.) (Point.from_coords (-3.) 2. 1.) !color in
  check testable_intersection "Ray & Plane intersect" Zero (intersects plane ray)


(******************)
(* TRIANGLE TESTS *)
(******************)

(* Ray passes through the triangle *)
let test_triangle_intersects _ = 
  let ray = { ray_origin = (Point.from_coords 0. 0. 0.); ray_direction = (Direction.from_coords 1. 1. 1.) } in
  let triangle = triangle (Point.from_coords 2. 0. 2.) (Point.from_coords 2. 2. 0.) (Point.from_coords 0. 2. 2.) !color in
  match triangle with
  | None -> failwith "bad triangle definition"
  | Some(t) -> check testable_intersection "Ray & Triangle dont intersect" !one_intersection (intersects t ray)

(* Ray doesn't pass through the triangle *)
let test_triangle_not_intersects _ = 
  let ray = { ray_origin = (Point.from_coords 0. 0. 0.); ray_direction = (Direction.from_coords 1. 1. 1.) } in
  let triangle = triangle (Point.from_coords 2. 0. 1.) (Point.from_coords 2. 2. 1.) (Point.from_coords 4. 0. 1.) !color in
  match triangle with
  | None -> failwith "bad triangle definition"
  | Some(t) -> check testable_intersection "Ray & Triangle intersect" Zero (intersects t ray)
  



let test_sphere = [
  "test_sphere_no_intersection", `Quick, test_sphere_no_intersection;
  "test_sphere_one_intersection", `Quick, test_sphere_one_intersection;
  "test_sphere_one_intersection_from_origin", `Quick, test_sphere_one_intersection_from_origin;
  "test_sphere_two_intersection", `Quick, test_sphere_two_intersection;
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