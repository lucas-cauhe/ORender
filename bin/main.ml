open Computer_gfx.Scene.Figures
open Computer_gfx.Geometry
open Computer_gfx.Colorspace
open Computer_gfx.Scene.Camera
open Computer_gfx.Obj_parser
open Computer_gfx.Bvh
open Computer_gfx.Scene.Light
open Computer_gfx.Common
open Computer_gfx.Algorithms
module PpmDb = Computer_gfx.Db.Ppm

let my_scene : scene =
  [ (* left *)
    Figure
      (plane
         (Direction.from_coords 1. 0. 0.)
         (Point.from_coords (-1.) 0. 0.)
         { emission = Rgb.rgb_of_values 0.75 0.75 0.75
         ; coefficients = Rgb.rgb_of_values 0.8 0.8 0.8, Rgb.zero (), Rgb.zero ()
         ; refraction = 1.
         })
  ; (* right *)
    Figure
      (plane
         (Direction.from_coords (-1.) 0. 0.)
         (Point.from_coords 1. 0. 0.)
         { emission = Rgb.rgb_of_values 0.75 0.75 0.75
         ; coefficients = Rgb.rgb_of_values 0.8 0.8 0.8, Rgb.zero (), Rgb.zero ()
         ; refraction = 1.
         })
  ; (* down *)
    Figure
      (plane
         (Direction.from_coords 0. 1. 0.)
         (Point.from_coords 0. (-1.) 0.)
         { emission = Rgb.rgb_of_values 0.75 0.75 0.75
         ; coefficients = Rgb.rgb_of_values 0.2 0.2 0.2, Rgb.zero (), Rgb.zero ()
         ; refraction = 1.
         })
  ; (* up *)
    Figure
      (plane
         (Direction.from_coords 0. (-1.) 0.)
         (Point.from_coords 0. 1. 0.)
         { emission = Rgb.rgb_of_values 0.75 0.75 0.75
         ; coefficients = Rgb.rgb_of_values 0.2 0.2 0.2, Rgb.zero (), Rgb.zero ()
         ; refraction = 1.
         })
  ; (* back *)
    Figure
      (plane
         (Direction.from_coords 0. 0. (-1.))
         (Point.from_coords 0. 0. 1.)
         { emission = Rgb.rgb_of_values 0.75 0.75 0.75
         ; coefficients = Rgb.rgb_of_values 0.2 0.2 0.2, Rgb.zero (), Rgb.zero ()
         ; refraction = 1.
         })
    (* triangle_box; *)
    (* ; Figure
      (sphere
         (Point.from_coords (-0.5) (-0.7) 0.25)
         0.3
         { emission = Rgb.rgb_of_values 0.3 0.9 0.95 (*Rgb.rgb_of_values 1. 1. 1.*)
         ; coefficients =
             (* Rgb.rgb_of_values 0.2 0.7 0.75, Rgb.rgb_of_values 0.6 0.2 0.15, Rgb.zero () *)
             Rgb.rgb_of_values 0.2 0.7 0.75, Rgb.zero (), Rgb.zero ()
             (* Rgb.zero (), Rgb.rgb_of_values 0.8 0.8 0.8, Rgb.rgb_of_values 0.8 0.8 0.8 *)
         ; refraction = 1.
         })
  ; Figure
      (sphere
         (Point.from_coords 0.5 (-0.7) (-0.25))
         0.3
         { emission = Rgb.rgb_of_values 0. 1. 0.5 (*Rgb.rgb_of_values 1. 1. 1.*)
         ; coefficients =
             (* Rgb.zero (), Rgb.zero (), Rgb.rgb_of_values 1. 1. 1. *)
             Rgb.rgb_of_values 0.5 0.8 0.6, Rgb.zero (), Rgb.zero ()
         ; refraction = 0.66
         }) *)
  ]
;;

let light_sources : light_source list =
  [ light_source (Point (Point.from_coords 0. 0.5 (-1.))) (Rgb.rgb_of_values 1. 1. 1.)
    (* [ light_source
      (Area
         (Figure
            (plane
               (Direction.from_coords 0. (-1.) 0.)
               (Point.from_coords 0. 10. 0.)
               { emission = Rgb.rgb_of_values 0.75 0.75 0.75
               ; coefficients = Rgb.rgb_of_values 0.8 0.8 0.8, Rgb.zero (), Rgb.zero ()
               ; refraction = 1.
               })))
      (Rgb.rgb_of_values 1. 1. 1.) *)
    (* light_source (Point(Point.from_coords 0.9 (-0.9) (-0.5))) (Rgb.rgb_of_values 1. 1. 1.) *)
  ]
;;

let left = ref (Direction.from_coords (-2.) 0. 0.)
(* let left = ref (Direction.from_coords (-20.) 0. 0.) *)

let up = ref (Direction.from_coords 0. 2. 0.)
(* let up = ref (Direction.from_coords 0. 20. 0.) *)

let forward = ref (Direction.from_coords 0. 0. 3.)
(* let forward = ref (Direction.from_coords 0. 0. 60.) *)

let origin = ref (Point.from_coords 0. 0. (-3.5))
(* let origin = ref (Point.from_coords 0. 0. (-60.5)) *)

let width, height = ref 512, ref 512

let texture_map_from_file file =
  let surface = Cairo.PNG.create file in
  let data = Cairo.Image.get_data32 surface in
  let width = Cairo.Image.get_width surface in
  let height = Cairo.Image.get_height surface in
  let stride = Cairo.Image.get_stride surface in
  Printf.printf "Texture map: width -> %d; height -> %d; stride -> %d" width height stride;
  { data; width; height; stride }
;;

let load_camel obj_file =
  let vertices, normals, faces, textures, kd, ks, ka = read_obj_file obj_file in
  let triangles = convert_to_scene (vertices, normals, faces, textures, kd, ks, ka) in
  let rotation_mat =
    Transformations.rotation_transformation_of_axis ~angle:(Float.pi /. 2.) Y
  in
  let triangles = rotate_mesh triangles rotation_mat Y in
  let scene_min, scene_max = scene_limits triangles in
  let scene_center = Point.mean [ scene_max; scene_min ] |> Option.get in
  let triangles = List.map (scale_figure 0.1 0.1 0.1 scene_center) triangles in
  let real_scene = split_scene triangles LargestAxis in
  let real_scene = translate_figure (-2.5) (-4.5) (-0.5) (List.nth real_scene 0) in
  [ real_scene ] @ my_scene
;;

(* let real_scene = translate_figure (-10.) (-6.) (-15.) (List.nth tri_scene 0) in *)
(* [ real_scene ] @ my_scene *)

let () =
  Random.self_init ();
  let camera = camera !up !left !forward !origin (!width, !height) in
  let oc = open_out "ppms/rendered/cornell.ppm" in
  let out_conf : PpmDb.config = PpmDb.config_of_values "P3" 1. 255 !width !height in
  PpmDb.write_header oc out_conf;
  (* let _ = random_walk my_scene light_sources 100000 pool in *)
  let my_scene = load_camel "obj_files/camel.obj" in
  let texture_map = texture_map_from_file "textures/camel.png" in
  color_image
    Pathtracing
    camera
    oc
    out_conf
    my_scene
    texture_map
    light_sources
    !width
    !height
;;
