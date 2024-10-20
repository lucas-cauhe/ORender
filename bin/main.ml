
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

module PpmDb = Computer_gfx.Db.Ppm

let red_sphere_center = Point.from_coords (-0.5) (-0.7) 0.25
let translation_mat = Matrix.from_array_matrix [|
  [| 1.; 0.; 0.; 1. |];
  [| 0.; 1.; 0.; 0.4 |];
  [| 0.; 0.; 1.; -0.5 |];
  [| 0.; 0.; 0.; 1. |];
|]
let red_sphere_translated_center = 
  match Transformations.hc_of_point red_sphere_center |> Transformations.translate translation_mat with
  | Some(new_point) -> Transformations.point_of_hc new_point
  | None -> red_sphere_center 


let triangle_rotation = Transformations.rotation_transformation_of_axis ~angle:(Float.pi/.4.) Z

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
  triangle (Point.from_coords 0. (-0.25) 0.8) (Point.from_coords (-0.25) 0.25 0.8) (Point.from_coords 0.25 0.25 0.8) (Rgb.rgb_of_values 0.75 0.75 0.) |> Option.get |> transform (Translation(0.5, 0.5,0.)) |> Option.get;
  triangle (Point.from_coords 0. (-0.25) 0.8) (Point.from_coords (-0.25) 0.25 0.8) (Point.from_coords 0.25 0.25 0.8) (Rgb.rgb_of_values 0. 0.75 0.75) |> Option.get |> transform (Scale(2., 2.,0.)) |> Option.get;
  triangle (Point.from_coords 0. (-0.25) 0.7) (Point.from_coords (-0.25) 0.25 0.7) (Point.from_coords 0.25 0.25 0.7) (Rgb.rgb_of_values 0.75 0. 0.75) |> Option.get |> transform (Rotation(triangle_rotation, Z)) |> Option.get;
  sphere red_sphere_translated_center 0.3 (Rgb.rgb_of_values 0.75 0. 0.);
  sphere (Point.from_coords 0.5 (-0.7) (-0.25)) 0.3 (Rgb.rgb_of_values 0. 0. 0.75);
]

let left = ref (Direction.from_coords (-2.) 0. 0.)
let up = ref (Direction.from_coords 0. 2. 0.)
let forward = ref (Direction.from_coords 0. 0. 3.)
let origin = ref (Point.from_coords 0. 0. (-3.5))
let width, height = ref 512, ref 512 

let () = 
  let camera = camera !up !left !forward !origin (!width,!height) in
  let oc = open_out "ppms/rendered/cornell.ppm" in
  let out_conf : PpmDb.config = PpmDb.config_of_values "P3" 1. 255 !height !width  in
  PpmDb.write_header oc out_conf;
  let pool = Task.setup_pool ~num_domains:7 () in
  let rec color_image row col = 
    match row, col with
    | r, _ when r = !height -> close_out oc 
    | _, _ -> begin
      let color = pixel_color camera (row, col) my_scene pool in
      PpmDb.write_pixel oc out_conf color;
      if col = !width - 1 then
        let () = output_string oc "\n" in
        color_image (row+1) 0
      else
        color_image row (col+1)
    end in
  color_image 0 0;
  Task.teardown_pool pool;
