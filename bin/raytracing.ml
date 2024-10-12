open Computer_gfx.Figures
open Computer_gfx.Geometry
open Computer_gfx.Colorspace


(* let my_scene : figure list = [
  Plane(Plane.init (Direction.from_coords 1. 1. 1.) (Point.from_coords 3. 0. 0.));
  Plane(Plane.init (Direction.from_coords 1. 1. 1.) (Point.from_coords 3. 0. 0.));
  Sphere(Sphere.init (Point.from_coords 6. 6. 6.) 3.)
]  *)

let my_scene : scene = List.init 100000 (fun _ -> plane (Direction.from_coords 1. 1. 1.) (Point.from_coords 3. 0. 0.) (Rgb.rgb_of_values 255. 0. 0. ))

let ray = ref { ray_direction = (Direction.from_coords 1. 1. 1.); ray_origin = (Point.from_coords 0. 0. 0.) }

(* let () = List.iter (Printf.printf "%f\n") (find_intersections my_scene !ray) *)
let () = match find_closest_figure (my_scene @ [plane (Direction.from_coords 1. 1. 1.) (Point.from_coords 0.5 0. 0.) (Rgb.rgb_of_values 255. 0. 0. ) ]) !ray with
| Some(fig) -> show_figure fig
| None -> print_endline "Ray doesn't intersect"
