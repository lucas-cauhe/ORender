
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
(* open Computer_gfx.Bvh *)
open Computer_gfx.Light

module PpmDb = Computer_gfx.Db.Ppm

(* let triangle_rotation = Transformations.rotation_transformation_of_axis ~angle:Float.pi Z

let triangle_box = BoundingBox(cuboid (Point.from_coords (-0.5) (-0.25) 0.7) (Point.from_coords 0.75 1. 0.8) (Rgb.rgb_of_values 0. 0. 0.), 
  [Figure(triangle (Point.from_coords 0. (-0.25) 0.8) (Point.from_coords (-0.25) 0.25 0.8) (Point.from_coords 0.25 0.25 0.8) (Rgb.rgb_of_values 0.75 0.75 0.) |> Option.get |> transform (Translation(0.5, 0.5,0.)) |> Option.get);
  Figure(triangle (Point.from_coords 0. (-0.25) 0.8) (Point.from_coords (-0.25) 0.25 0.8) (Point.from_coords 0.25 0.25 0.8) (Rgb.rgb_of_values 0. 0.75 0.75) |> Option.get |> transform (Scale(2., 2.,0.)) |> Option.get);
  Figure(triangle (Point.from_coords 0. (-0.25) 0.7) (Point.from_coords (-0.25) 0.25 0.7) (Point.from_coords 0.25 0.25 0.7) (Rgb.rgb_of_values 0.75 0. 0.75) |> Option.get |> transform (Rotation(triangle_rotation, Z)) |> Option.get);]) *)

(* let triangle_set : scene = List.init 1000 (fun _  -> Figure(triangle (Point.from_coords 0. (-0.25) 0.8) (Point.from_coords (-0.25) 0.25 0.8) (Point.from_coords 0.25 0.25 0.8) (Rgb.rgb_of_values 0.75 0.75 0.) |> Option.get)) *)

(* let triangle_mesh : scene = [
  Figure(triangle (Point.from_coords (-0.75) (-0.25) (-0.25)) (Point.from_coords (-0.7) (-0.2) (-0.25)) (Point.from_coords (-0.65) (-0.25) (-0.25)) (Rgb.rgb_of_values 0.75 0.75 0.) ~coefficients:(Rgb.rgb_of_values 0.75 0.75 0.) |> Option.get );
  Figure(triangle (Point.from_coords (-0.75) (-0.25) 0.) (Point.from_coords (-0.7) (-0.2) 0.) (Point.from_coords (-0.65) (-0.25) 0.) (Rgb.rgb_of_values 0.75 0.75 0.) ~coefficients:(Rgb.rgb_of_values 0.75 0.75 0.) |> Option.get );
  Figure(triangle (Point.from_coords (-0.75) (-0.25) 0.25) (Point.from_coords (-0.7) (-0.2) 0.25) (Point.from_coords (-0.65) (-0.25) 0.25) (Rgb.rgb_of_values 0.75 0.75 0.) ~coefficients:(Rgb.rgb_of_values 0.75 0.75 0.) |> Option.get );
  Figure(triangle (Point.from_coords (-0.75) 0.25 (-0.25)) (Point.from_coords (-0.7) 0.3 (-0.25)) (Point.from_coords (-0.65) 0.25 (-0.25)) (Rgb.rgb_of_values 0.75 0.75 0.) ~coefficients:(Rgb.rgb_of_values 0.75 0.75 0.) |> Option.get );
  Figure(triangle (Point.from_coords (-0.75) 0.25 0.) (Point.from_coords (-0.7) 0.3 0.) (Point.from_coords (-0.65) 0.25 0.) (Rgb.rgb_of_values 0.75 0.75 0.) ~coefficients:(Rgb.rgb_of_values 0.75 0.75 0.) |> Option.get );
  Figure(triangle (Point.from_coords (-0.75) 0.25 0.25) (Point.from_coords (-0.7) 0.3 0.25) (Point.from_coords (-0.65) 0.25 0.25) (Rgb.rgb_of_values 0.75 0.75 0.) ~coefficients:(Rgb.rgb_of_values 0.75 0.75 0.) |> Option.get );

  Figure(triangle (Point.from_coords (-0.25) (-0.25) (-0.25)) (Point.from_coords (-0.2) (-0.2) (-0.25)) (Point.from_coords (-0.15) (-0.25) (-0.25)) (Rgb.rgb_of_values 0.75 0.75 0.) ~coefficients:(Rgb.rgb_of_values 0.75 0.75 0.) |> Option.get );
  Figure(triangle (Point.from_coords (-0.25) (-0.25) 0.) (Point.from_coords (-0.2) (-0.2) 0.) (Point.from_coords (-0.15) (-0.25) 0.) (Rgb.rgb_of_values 0.75 0.75 0.) ~coefficients:(Rgb.rgb_of_values 0.75 0.75 0.) |> Option.get );
  Figure(triangle (Point.from_coords (-0.25) (-0.25) 0.25) (Point.from_coords (-0.2) (-0.2) 0.25) (Point.from_coords (-0.15) (-0.25) 0.25) (Rgb.rgb_of_values 0.75 0.75 0.) ~coefficients:(Rgb.rgb_of_values 0.75 0.75 0.) |> Option.get );
  Figure(triangle (Point.from_coords (-0.25) 0.25 (-0.25)) (Point.from_coords (-0.2) 0.3 (-0.25)) (Point.from_coords (-0.15) 0.25 (-0.25)) (Rgb.rgb_of_values 0.75 0.75 0.) ~coefficients:(Rgb.rgb_of_values 0.75 0.75 0.) |> Option.get );
  Figure(triangle (Point.from_coords (-0.25) 0.25 0.) (Point.from_coords (-0.2) 0.3 0.) (Point.from_coords (-0.15) 0.25 0.) (Rgb.rgb_of_values 0.75 0.75 0.) ~coefficients:(Rgb.rgb_of_values 0.75 0.75 0.) |> Option.get );
  Figure(triangle (Point.from_coords (-0.25) 0.25 0.25) (Point.from_coords (-0.2) 0.3 0.25) (Point.from_coords (-0.15) 0.25 0.25) (Rgb.rgb_of_values 0.75 0.75 0.) ~coefficients:(Rgb.rgb_of_values 0.75 0.75 0.) |> Option.get );

  Figure(triangle (Point.from_coords 0. (-0.25) (-0.25)) (Point.from_coords 0.2 (-0.2) (-0.25)) (Point.from_coords 0.25 (-0.25) (-0.25)) (Rgb.rgb_of_values 0.75 0.75 0.) ~coefficients:(Rgb.rgb_of_values 0.75 0.75 0.) |> Option.get );
  Figure(triangle (Point.from_coords 0. (-0.25) 0.) (Point.from_coords 0.2 (-0.2) 0.) (Point.from_coords 0.25 (-0.25) 0.) (Rgb.rgb_of_values 0.75 0.75 0.) ~coefficients:(Rgb.rgb_of_values 0.75 0.75 0.) |> Option.get );
  Figure(triangle (Point.from_coords 0. (-0.25) 0.25) (Point.from_coords 0.2 (-0.2) 0.25) (Point.from_coords 0.25 (-0.25) 0.25) (Rgb.rgb_of_values 0.75 0.75 0.) ~coefficients:(Rgb.rgb_of_values 0.75 0.75 0.) |> Option.get );
  Figure(triangle (Point.from_coords 0. 0.25 (-0.25)) (Point.from_coords 0.2 0.3 (-0.25)) (Point.from_coords 0.25 0.25 (-0.25)) (Rgb.rgb_of_values 0.75 0.75 0.) ~coefficients:(Rgb.rgb_of_values 0.75 0.75 0.) |> Option.get );
  Figure(triangle (Point.from_coords 0. 0.25 0.) (Point.from_coords 0.2 0.3 0.) (Point.from_coords 0.25 0.25 0.) (Rgb.rgb_of_values 0.75 0.75 0.) ~coefficients:(Rgb.rgb_of_values 0.75 0.75 0.) |> Option.get );
  Figure(triangle (Point.from_coords 0. 0.25 0.25) (Point.from_coords 0.2 0.3 0.25) (Point.from_coords 0.25 0.25 0.25) (Rgb.rgb_of_values 0.75 0.75 0.) ~coefficients:(Rgb.rgb_of_values 0.75 0.75 0.) |> Option.get );

  Figure(triangle (Point.from_coords 0.5 (-0.25) (-0.25)) (Point.from_coords 0.55 (-0.2) (-0.25)) (Point.from_coords 0.65 (-0.25) (-0.25)) (Rgb.rgb_of_values 0.75 0.75 0.) ~coefficients:(Rgb.rgb_of_values 0.75 0.75 0.) |> Option.get );
  Figure(triangle (Point.from_coords 0.5 (-0.25) 0.) (Point.from_coords 0.55 (-0.2) 0.) (Point.from_coords 0.65 (-0.25) 0.) (Rgb.rgb_of_values 0.75 0.75 0.) ~coefficients:(Rgb.rgb_of_values 0.75 0.75 0.) |> Option.get );
  Figure(triangle (Point.from_coords 0.5 (-0.25) 0.25) (Point.from_coords 0.55 (-0.2) 0.25) (Point.from_coords 0.65 (-0.25) 0.25) (Rgb.rgb_of_values 0.75 0.75 0.) ~coefficients:(Rgb.rgb_of_values 0.75 0.75 0.) |> Option.get );
  Figure(triangle (Point.from_coords 0.5 0.25 (-0.25)) (Point.from_coords 0.55 0.3 (-0.25)) (Point.from_coords 0.65 0.25 (-0.25)) (Rgb.rgb_of_values 0.75 0.75 0.) ~coefficients:(Rgb.rgb_of_values 0.75 0.75 0.) |> Option.get );
  Figure(triangle (Point.from_coords 0.5 0.25 0.) (Point.from_coords 0.55 0.3 0.) (Point.from_coords 0.65 0.25 0.) (Rgb.rgb_of_values 0.75 0.75 0.) ~coefficients:(Rgb.rgb_of_values 0.75 0.75 0.) |> Option.get );
  Figure(triangle (Point.from_coords 0.5 0.25 0.25) (Point.from_coords 0.55 0.3 0.25) (Point.from_coords 0.65 0.25 0.25) (Rgb.rgb_of_values 0.75 0.75 0.) ~coefficients:(Rgb.rgb_of_values 0.75 0.75 0.) |> Option.get );
]

let new_triangle_mesh = split_scene triangle_mesh LargestAxis  *)

(* let () =
  let rec show_scene scene =
    match scene with
    | [] -> ()
    | fig :: rest -> 
      match fig with 
      | Figure(f) -> show_figure f; show_scene rest 
      | BoundingBox(_, children) -> 
        print_endline "--- BOUNDING BOX ---";
        show_scene children;
        print_endline "--- END OF BBOX ---";
        show_scene rest in
  show_scene new_triangle_mesh *)

let my_scene : scene = [
  (* left *)
  Figure(plane (Direction.from_coords 1. 0. 0.) (Point.from_coords (-1.) 0. 0.) (Rgb.rgb_of_values 1. 0. 0.) ~coefficients:(Rgb.rgb_of_values 1. 0. 0., Rgb.zero(), Rgb.zero()));
  (* right *)
  Figure(plane (Direction.from_coords (-1.) 0. 0.) (Point.from_coords 1. 0. 0.) (Rgb.rgb_of_values 0. 1. 0.) ~coefficients:(Rgb.rgb_of_values 0. 1. 0., Rgb.zero(), Rgb.zero()));
  (* down *)
  Figure(plane (Direction.from_coords 0. 1. 0.) (Point.from_coords 0. (-1.) 0.) (Rgb.rgb_of_values 0.75 0.75 0.75) ~coefficients:(Rgb.rgb_of_values 1. 1. 1., Rgb.zero(), Rgb.zero()));
  (* up *)
  Figure(plane (Direction.from_coords 0. (-1.) 0.) (Point.from_coords 0. 1. 0.) (Rgb.rgb_of_values 0.75 0.75 0.75) ~coefficients:(Rgb.rgb_of_values 1. 1. 1., Rgb.zero(), Rgb.zero()));
  (* back *)
  Figure(plane (Direction.from_coords 0. 0. (-1.)) (Point.from_coords 0. 0. 1.) (Rgb.rgb_of_values 0.75 0.75 0.75) ~coefficients:(Rgb.rgb_of_values 1. 1. 1., Rgb.zero(), Rgb.zero()));
  (* triangle_box; *)
  Figure(sphere (Point.from_coords (-0.5) (-0.7) 0.25) 0.3 (Rgb.rgb_of_values 1. 1. 1.) ~coefficients:(Rgb.rgb_of_values 1. 1. 1., Rgb.zero(), Rgb.zero()) (*|> transform (Translation(0.5, 0.4, (-0.25))) |> Option.get*) );
  Figure(sphere (Point.from_coords 0.5 (-0.7) (-0.25)) 0.3 (Rgb.rgb_of_values 1. 1. 1.) ~coefficients:(Rgb.rgb_of_values 1. 1. 1., Rgb.zero(), Rgb.zero()));
]

let light_sources : light_source list = [
  light_source (Point(Point.from_coords 0. 0.9 0.)) (Rgb.rgb_of_values 1. 1. 1.);
  (* light_source (Area(Figure(plane (Direction.from_coords 0. (-1.) 0.) (Point.from_coords 0. 1. 0.) (Rgb.rgb_of_values 0.75 0.75 0.75) ~coefficients:(Rgb.rgb_of_values 0.5 0. 0.)))) (Rgb.rgb_of_values 1. 1. 1.)  *)
  (* light_source (Point(Point.from_coords 0.9 (-0.9) (-0.5))) (Rgb.rgb_of_values 1. 1. 1.) *)
]

let left = ref (Direction.from_coords (-2.) 0. 0.)
let up = ref (Direction.from_coords 0. 2. 0.)
let forward = ref (Direction.from_coords 0. 0. 3.)
let origin = ref (Point.from_coords 0. 0. (-3.5))
let width, height = ref 512, ref 512 

let bar ~total = 
  let open Progress.Line in
  list [ spinner (); bar total; count_to total ] 

let () = 
  let camera = camera !up !left !forward !origin (!width,!height) in
  let oc = open_out "ppms/rendered/cornell.ppm" in
  let out_conf : PpmDb.config = PpmDb.config_of_values "P3" 1. 255 !height !width  in
  PpmDb.write_header oc out_conf;
  let pool = Task.setup_pool ~num_domains:7 () in
  let rec color_image row col reporter = 
    match row, col with
    | r, _ when r = !height -> close_out oc 
    | _, _ -> begin
      let color = pixel_color camera (row, col) my_scene light_sources pool in
      reporter 1;
      PpmDb.write_pixel oc out_conf color;
      if col = !width - 1 then
        let () = output_string oc "\n" in
        color_image (row+1) 0 reporter
      else
        color_image row (col+1) reporter
    end in
  Progress.with_reporter (bar ~total:(!width * !height)) (fun f -> color_image 0 0 f);
  Task.teardown_pool pool;
