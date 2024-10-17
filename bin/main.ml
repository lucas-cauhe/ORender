
(* let test_tonemap () =
  (* Iter.(IO.chunks_of "forest_path.ppm" |> filter (fun l -> l <> "") |> IO.write_lines "forest_path_out.ppm") *)
  let ppm_colors: rgb list = read_ppm "forest_path.ppm" in
  List.map tone_map ppm_colors |> Iter.IO.write_lines "forest_path_out.ppm"

in *)
open Domainslib

open Computer_gfx.Figures
open Computer_gfx.Geometry
open Computer_gfx.Colorspace
open Computer_gfx.Camera

let my_scene : scene = [
  (* left *)
  plane (Direction.from_coords 1. 0. 0.) (Point.from_coords (-1.) 0. 0.) (Rgb.rgb_of_values 1. 0. 0.);
  (* right *)
  plane (Direction.from_coords (-1.) 0. 0.) (Point.from_coords 1. 0. 0.) (Rgb.rgb_of_values 0. 1. 0.);
  (* down *)
  plane (Direction.from_coords 0. 1. 0.) (Point.from_coords 0. (-1.) 0.) (Rgb.rgb_of_values 0.75 0.25 0.25);
  (* up *)
  plane (Direction.from_coords 0. (-1.) 0.) (Point.from_coords 0. 1. 0.) (Rgb.rgb_of_values 0.25 0.75 0.25);
  (* back *)
  plane (Direction.from_coords 0. 0. (-1.)) (Point.from_coords 0. 0. 1.) (Rgb.rgb_of_values 0.25 0.25 0.75);
  sphere (Point.from_coords (-0.5) (-0.7) 0.25) 0.3 (Rgb.rgb_of_values 0.75 0. 0.);
  sphere (Point.from_coords 0.5 (-0.7) (-0.25)) 0.3 (Rgb.rgb_of_values 0. 0. 0.75);
]

module PpmDb = Computer_gfx.Db.Ppm
let left = ref (Direction.from_coords 0. (-1.) 0.)
let up = ref (Direction.from_coords (-1.) 0. 0.)
let forward = ref (Direction.from_coords 0. 0. (-6.))
let origin = ref (Point.from_coords 0. 0. (-10.5))


let () = 
  let camera = camera !up !left !forward !origin in
  let oc = open_out "ppms/rendered/cornell.ppm" in
  let out_conf : PpmDb.config = PpmDb.config_of_values "P3" 1. 255 256 256  in
  PpmDb.write_header oc out_conf;
  let pool = Task.setup_pool ~num_domains:7 () in
  let rec color_image row col = 
    match row, col with
    | 256, _ -> close_out oc 
    | _, _ -> begin
      let color = trace_ray camera (row, col) my_scene pool in
      PpmDb.write_pixel oc out_conf color;
      if col = 255 then
        let () = output_string oc "\n" in
        color_image (row+1) 0
      else
        color_image row (col+1)
    end in
  color_image 0 0;
  Task.teardown_pool pool;
