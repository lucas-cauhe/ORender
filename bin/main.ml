(* let test_tonemap () =
   (* Iter.(IO.chunks_of "forest_path.ppm" |> filter (fun l -> l <> "") |> IO.write_lines "forest_path_out.ppm") *)
   let ppm_colors: rgb list = read_ppm "forest_path.ppm" in
   List.map tone_map ppm_colors |> Iter.IO.write_lines "forest_path_out.ppm"

   in *)
open Domainslib
open Computer_gfx.Scene.Figures
open Computer_gfx.Geometry
module Photon = Computer_gfx.Photon
open Computer_gfx.Colorspace
open Computer_gfx.Scene.Camera
open Computer_gfx.Obj_parser
open Computer_gfx.Photonmap

(* open Computer_gfx.Bvh *)
open Computer_gfx.Scene.Light
open Computer_gfx.Pathtracing
module PpmDb = Computer_gfx.Db.Ppm

let my_scene : scene =
  [ (* left *)
    Figure
      (plane
         (Direction.from_coords 1. 0. 0.)
         (Point.from_coords (-10.) 0. 0.)
         { emission = Rgb.rgb_of_values 0.75 0.75 0.75
         ; coefficients = Rgb.rgb_of_values 0.8 0.8 0.8, Rgb.zero (), Rgb.zero ()
         ; refraction = 1.
         })
  ; (* right *)
    Figure
      (plane
         (Direction.from_coords (-1.) 0. 0.)
         (Point.from_coords 10. 0. 0.)
         { emission = Rgb.rgb_of_values 0. 1. 0.
         ; coefficients = Rgb.rgb_of_values 0.0 0.8 0.0, Rgb.zero (), Rgb.zero ()
         ; refraction = 1.
         })
  ; (* down *)
    Figure
      (plane
         (Direction.from_coords 0. 1. 0.)
         (Point.from_coords 0. (-10.) 0.)
         { emission = Rgb.rgb_of_values 0.75 0.75 0.75
         ; coefficients = Rgb.rgb_of_values 0.75 0.75 0.75, Rgb.zero (), Rgb.zero ()
         ; refraction = 1.
         })
  ; (* up *)
    Figure
      (plane
         (Direction.from_coords 0. (-1.) 0.)
         (Point.from_coords 0. 20. 0.)
         { emission = Rgb.rgb_of_values 0.75 0.75 0.75
         ; coefficients = Rgb.rgb_of_values 0.75 0.75 0.75, Rgb.zero (), Rgb.zero ()
         ; refraction = 1.
         })
  ; (* back *)
    Figure
      (plane
         (Direction.from_coords 0. 0. (-1.))
         (Point.from_coords 0. 0. 10.)
         { emission = Rgb.rgb_of_values 0.75 0.75 0.75
         ; coefficients = Rgb.rgb_of_values 0.8 0.8 0.8, Rgb.zero (), Rgb.zero ()
         ; refraction = 1.
         })
    (* triangle_box; *)
    (* Figure
      (sphere
         (Point.from_coords (-0.5) (-0.7) 0.25)
         0.3
         { emission = Rgb.rgb_of_values 0.3 0.9 0.95
         ; coefficients =
             Rgb.rgb_of_values 0.2 0.7 0.75, Rgb.rgb_of_values 0.6 0.2 0.15, Rgb.zero ()
         ; refraction = 1.
         })
    (*|> transform (Translation(0.5, 0.4, (-0.25))) |> Option.get*)
  ; Figure
      (sphere
         (Point.from_coords 0.5 (-0.7) (-0.25))
         0.3
         (* (Rgb.rgb_of_values 0. 1. 0.5) *)
         { emission = Rgb.rgb_of_values 1. 1. 1.
         ; coefficients =
             Rgb.zero (), Rgb.rgb_of_values 0.2 0.2 0.2, Rgb.rgb_of_values 1. 1. 1.
             (* Rgb.rgb_of_values 0.5 0.8 0.6, Rgb.zero (), Rgb.zero () *)
         ; refraction = 0.667
         }) *)
  ]
;;

let light_sources : light_source list =
  (* [ light_source (Point (Point.from_coords 0. 0.5 0.)) (Rgb.rgb_of_values 1. 1. 1.) *)
  [ light_source
      (Area
         (Figure
            (plane
               (Direction.from_coords 0. (-1.) 0.)
               (Point.from_coords 0. 10. 0.)
               { emission = Rgb.rgb_of_values 0.75 0.75 0.75
               ; coefficients = Rgb.rgb_of_values 0.8 0.8 0.8, Rgb.zero (), Rgb.zero ()
               ; refraction = 1.
               })))
      (Rgb.rgb_of_values 1. 1. 1.)
    (* light_source (Point(Point.from_coords 0.9 (-0.9) (-0.5))) (Rgb.rgb_of_values 1. 1. 1.) *)
  ]
;;

let left = ref (Direction.from_coords (-20.) 0. 0.)
let up = ref (Direction.from_coords 0. 20. 0.)
let forward = ref (Direction.from_coords 0. 0. 10.)
let origin = ref (Point.from_coords 0. 0. (-10.5))
(* let width, height = ref 1024, ref 576 *)

let width, height = ref 512, ref 512
let num_points = ref 16

let bar ~total =
  let open Progress.Line in
  list [ spinner (); bar total; count_to total ]
;;

let _load_camel obj_file =
  let vertices, _, faces = read_obj_file obj_file in
  (* List.iter (fun face -> Printf.printf "Face -> %d %d %d\n" face.v1 face.v2 face.v3) faces; *)
  let triangles = convert_to_scene (vertices, faces) in
  (* let tri_scene = split_scene triangles LargestAxis in *)
  triangles @ my_scene
;;

let photonmap_pixel_color cam (row, col) scene photons pool =
  let pip_arr = BatArray.of_list (points_in_pixel cam (row, col) !num_points) in
  let compute_pixel_color ind =
    Direction.between_points pip_arr.(ind) (cam_origin cam)
    |> Direction.normalize
    |> Option.get
    |> ray (cam_origin cam)
    |> photonmap scene photons
  in
  let color_sum () =
    Task.parallel_for_reduce
      ~start:0
      ~finish:(BatArray.length pip_arr - 1)
      ~body:compute_pixel_color
      pool
      (fun acc next_color -> Rgb.sum acc next_color)
      (Rgb.zero ())
  in
  Rgb.normalize (Task.run pool (fun _ -> color_sum ())) (float_of_int !num_points)
;;

let _pathtracing_pixel_color cam (row, col) scene light_sources pool =
  let pip_arr = BatArray.of_list (points_in_pixel cam (row, col) !num_points) in
  let compute_pixel_color ind =
    Direction.between_points pip_arr.(ind) (cam_origin cam)
    |> Direction.normalize
    |> Option.get
    |> ray (cam_origin cam)
    |> path_tracing scene light_sources
  in
  let color_sum () =
    Task.parallel_for_reduce
      ~start:0
      ~finish:(BatArray.length pip_arr - 1)
      ~body:compute_pixel_color
      pool
      (fun acc next_color -> Rgb.sum acc next_color)
      (Rgb.zero ())
  in
  Rgb.normalize (Task.run pool (fun _ -> color_sum ())) (float_of_int !num_points)
;;

let () =
  Random.self_init ();
  let camera = camera !up !left !forward !origin (!width, !height) in
  let oc = open_out "ppms/rendered/cornell.ppm" in
  let out_conf : PpmDb.config = PpmDb.config_of_values "P3" 1. 255 !width !height in
  PpmDb.write_header oc out_conf;
  let photons = random_walk my_scene light_sources 10 in
  let pool = Task.setup_pool ~num_domains:7 () in
  (* let my_scene = load_camel "obj_files/camel.obj" in *)
  let rec color_image row col reporter =
    match row, col with
    | r, _ when r = !height -> close_out oc
    | _, _ ->
      (* let color = pathtracing_pixel_color camera (row, col) my_scene light_sources pool in *)
      let color = photonmap_pixel_color camera (row, col) my_scene photons pool in
      reporter 1;
      PpmDb.write_pixel oc out_conf color;
      if col >= !width - 1 then (
        let () = output_string oc "\n" in
        color_image (row + 1) 0 reporter
      ) else
        color_image row (col + 1) reporter
  in
  Progress.with_reporter (bar ~total:(!width * !height)) (fun f -> color_image 0 0 f);
  Task.teardown_pool pool
;;
