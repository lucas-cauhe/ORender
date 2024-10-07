open Computer_gfx.Figures
open Computer_gfx.Geometry


(* let my_scene : figure list = [
  Plane(Plane.init (Direction.from_coords 1. 1. 1.) (Point.from_coords 3. 0. 0.));
  Plane(Plane.init (Direction.from_coords 1. 1. 1.) (Point.from_coords 3. 0. 0.));
  Sphere(Sphere.init (Point.from_coords 6. 6. 6.) 3.)
]  *)

let my_scene : scene = List.init 100000 (fun _ -> Plane(Plane.init (Direction.from_coords 1. 1. 1.) (Point.from_coords 3. 0. 0.)))

let ray = ref { direction = (Direction.from_coords 1. 1. 1.); origin = (Point.from_coords 0. 0. 0.) }

(* let find_intersections (scene : figure list) (ray : ray_type) =
  let rec loop_ intersections scene =
    match scene with
    | [] -> intersections
    | Plane(config) :: rest -> loop_ (intersections @ (Plane.intersects config ray)) rest
    | Sphere(config) :: rest -> loop_ (intersections @ (Sphere.intersects config ray)) rest
  in
  loop_ [] scene *)

(* let () = List.iter (Printf.printf "%f\n") (find_intersections my_scene !ray) *)
let () = find_closest_figure (my_scene @ [Plane(Plane.init (Direction.from_coords 1. 1. 1.) (Point.from_coords 0.5 0. 0.))]) !ray |> show_figure
