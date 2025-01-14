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

let basic_cuboid = cuboid (Point.from_coords 0. 0. 0.) (Point.from_coords 0.3 0.3 0.3)

let diffuse_cuboid =
  basic_cuboid
    { emission = Rgb.rgb_of_values 0.8 0. 0.
    ; coefficients = Rgb.rgb_of_values 0.8 0. 0., Rgb.zero (), Rgb.zero ()
    ; refraction = 1.
    }
;;

let specular_cuboid =
  basic_cuboid
    { emission = Rgb.rgb_of_values 1. 1. 1.
    ; coefficients = Rgb.zero (), Rgb.rgb_of_values 0.8 0.8 0.8, Rgb.zero ()
    ; refraction = 0.66
    }
;;

let my_scene : scene =
  [ (* left *)
    Figure
      (plane
         (Direction.from_coords 1. 0. 0.)
         (Point.from_coords (-2.) 0. 0.)
         { emission = Rgb.rgb_of_values 0.75 0. 0.
         ; coefficients = Rgb.rgb_of_values 0.8 0. 0., Rgb.zero (), Rgb.zero ()
         ; refraction = 1.
         })
  ; (* right *)
    Figure
      (plane
         (Direction.from_coords (-1.) 0. 0.)
         (Point.from_coords 2. 0. 0.)
         { emission = Rgb.rgb_of_values 0. 0.75 0.
         ; coefficients = Rgb.rgb_of_values 0. 0.8 0., Rgb.zero (), Rgb.zero ()
         ; refraction = 1.
         })
  ; (* down *)
    Figure
      (plane
         (Direction.from_coords 0. 1. 0.)
         (Point.from_coords 0. (-2.) 0.)
         { emission = Rgb.rgb_of_values 0.25 0.44 0.89
         ; coefficients =
             Rgb.rgb_of_values 0.5 0.5 0.5, Rgb.rgb_of_values 0.5 0.5 0.5, Rgb.zero ()
         ; refraction = 1.
         })
  ; (* up *)
    Figure
      (plane
         (Direction.from_coords 0. (-1.) 0.)
         (Point.from_coords 0. 2. 0.)
         { emission = Rgb.rgb_of_values 0.75 0.75 0.75
         ; coefficients = Rgb.rgb_of_values 0.5 0.5 0.5, Rgb.zero (), Rgb.zero ()
         ; refraction = 1.
         })
  ; (* back *)
    Figure
      (plane
         (Direction.from_coords 0. 0. (-1.))
         (Point.from_coords 0. 0. 2.)
         { emission = Rgb.rgb_of_values 0.75 0.75 0.75
         ; coefficients = Rgb.zero (), Rgb.rgb_of_values 0.8 0.8 0.8, Rgb.zero ()
         ; refraction = 1.
         })
  ; Figure (transform (Translation (-1.5, -1.7, -1.)) specular_cuboid |> Option.get)
  ; Figure (transform (Translation (-1.5, -1.4, -1.)) diffuse_cuboid |> Option.get)
  ; Figure
      (sphere
         (Point.from_coords (-1.35) (-0.9) (-0.9))
         0.2
         { emission = (*Rgb.rgb_of_values 0.3 0.6 0.95*) Rgb.rgb_of_values 1. 1. 1.
         ; coefficients =
             (* Rgb.rgb_of_values 0.3 0.5 0.75, Rgb.rgb_of_values 0.2 0.2 0.2, Rgb.zero () *)
             (* Rgb.rgb_of_values 0.7 0.4 0.25, Rgb.zero (), Rgb.zero () *)
             Rgb.zero (), Rgb.rgb_of_values 0.8 0.8 0.8, Rgb.rgb_of_values 0.8 0.8 0.8
         ; refraction = 0.66
         })
  ]
;;

let light_sources : light_source list =
  (* [ light_source (Point (Point.from_coords 0. 1. 0.)) (Rgb.rgb_of_values 1. 1. 1.) *)
  [ light_source
      (Area
         (Figure
            (plane
               (Direction.from_coords 0. (-1.) 0.)
               (Point.from_coords 0. 2. 0.)
               { emission = Rgb.rgb_of_values 0.75 0.75 0.75
               ; coefficients = Rgb.rgb_of_values 0.8 0.8 0.8, Rgb.zero (), Rgb.zero ()
               ; refraction = 1.
               })))
      (Rgb.rgb_of_values 0.042 0.065 0.094)
  ; light_source
      (Area
         (Figure
            (plane
               (Direction.from_coords 0. 0. 1.)
               (Point.from_coords 0. 0. (-21.98))
               { emission = Rgb.rgb_of_values 0.75 0.75 0.75
               ; coefficients = Rgb.rgb_of_values 0.8 0.8 0.8, Rgb.zero (), Rgb.zero ()
               ; refraction = 1.
               })))
      (Rgb.rgb_of_values 1.042 1.065 1.094)
  ; light_source
      (Area
         (Figure
            (cuboid
               (Point.from_coords (-2.) (-1.) (-4.5))
               (Point.from_coords (-1.99) 0.5 0.)
               { emission = Rgb.zero ()
               ; coefficients = Rgb.zero (), Rgb.zero (), Rgb.zero ()
               ; refraction = 1.
               })))
      (Rgb.rgb_of_values 2. 2. 2.)
  ; light_source
      (Area
         (Figure
            (cuboid
               (Point.from_coords 1.99 (-1.) (-4.5))
               (Point.from_coords 2. 0.5 0.)
               { emission = Rgb.zero ()
               ; coefficients = Rgb.zero (), Rgb.zero (), Rgb.zero ()
               ; refraction = 1.
               })))
      (Rgb.rgb_of_values 2. 2. 2.)
  ; light_source
      (Area
         (Figure
            (sphere
               (Point.from_coords 0. 2. 0.)
               0.4
               { emission = Rgb.rgb_of_values 0.3 0.6 0.95 (*Rgb.rgb_of_values 1. 1. 1.*)
               ; coefficients =
                   ( Rgb.rgb_of_values 0.3 0.5 0.75
                   , Rgb.rgb_of_values 0.2 0.2 0.2
                   , Rgb.zero () )
                   (* Rgb.rgb_of_values 0.7 0.4 0.25, Rgb.zero (), Rgb.zero () *)
                   (* Rgb.zero (), Rgb.rgb_of_values 0.8 0.8 0.8, Rgb.rgb_of_values 0.8 0.8 0.8 *)
               ; refraction = 1.
               })))
      (Rgb.rgb_of_values 7.84 5.76 0.08)
  ]
  (* @ List.map
      (fun i ->
        light_source
          (Area
             (Figure
                (sphere
                   (Point.from_coords 0. 2. (-4. *. i))
                   0.4
                   { emission =
                       Rgb.rgb_of_values 0.3 0.6 0.95 (*Rgb.rgb_of_values 1. 1. 1.*)
                   ; coefficients =
                       ( Rgb.rgb_of_values 0.3 0.5 0.75
                       , Rgb.rgb_of_values 0.2 0.2 0.2
                       , Rgb.zero () )
                       (* Rgb.rgb_of_values 0.7 0.4 0.25, Rgb.zero (), Rgb.zero () *)
                       (* Rgb.zero (), Rgb.rgb_of_values 0.8 0.8 0.8, Rgb.rgb_of_values 0.8 0.8 0.8 *)
                   ; refraction = 1.
                   })))
          (Rgb.rgb_of_values 7.84 5.76 0.08))
      [ 0.; 1.; 2.; 3.; 4.; 5. ] *)
  @ List.map
      (fun i ->
        light_source
          (Area
             (Figure
                (sphere
                   (Point.from_coords 1.9 1.9 (1.9 -. (2. *. i *. 1.9)))
                   0.1
                   { emission =
                       Rgb.rgb_of_values 0.3 0.6 0.95 (*Rgb.rgb_of_values 1. 1. 1.*)
                   ; coefficients =
                       ( Rgb.rgb_of_values 0.3 0.5 0.75
                       , Rgb.rgb_of_values 0.2 0.2 0.2
                       , Rgb.zero () )
                       (* Rgb.rgb_of_values 0.7 0.4 0.25, Rgb.zero (), Rgb.zero () *)
                       (* Rgb.zero (), Rgb.rgb_of_values 0.8 0.8 0.8, Rgb.rgb_of_values 0.8 0.8 0.8 *)
                   ; refraction = 1.
                   })))
          (Rgb.rgb_of_values 3.32 1. 3.56))
      [ 0.; 1.; 2.; 3.; 4.; 5.; 6. ]
  @ List.map
      (fun i ->
        light_source
          (Area
             (Figure
                (sphere
                   (Point.from_coords (-1.9) 1.9 (1.9 -. (2. *. i *. 1.9)))
                   0.1
                   { emission =
                       Rgb.rgb_of_values 0.3 0.6 0.95 (*Rgb.rgb_of_values 1. 1. 1.*)
                   ; coefficients =
                       ( Rgb.rgb_of_values 0.3 0.5 0.75
                       , Rgb.rgb_of_values 0.2 0.2 0.2
                       , Rgb.zero () )
                       (* Rgb.rgb_of_values 0.7 0.4 0.25, Rgb.zero (), Rgb.zero () *)
                       (* Rgb.zero (), Rgb.rgb_of_values 0.8 0.8 0.8, Rgb.rgb_of_values 0.8 0.8 0.8 *)
                   ; refraction = 1.
                   })))
          (Rgb.rgb_of_values 3.32 1. 3.56))
      [ 0.; 1.; 2.; 3.; 4.; 5.; 6. ]
;;

let left = ref (Direction.from_coords (-5.) 0. 0.)
let up = ref (Direction.from_coords 0. 5. 0.)
let forward = ref (Direction.from_coords 0. 0. 15.)
let origin = ref (Point.from_coords 0. 0. (-15.5))
let width, height = ref 1024, ref 576

let texture_map_from_file file =
  let surface = Cairo.PNG.create file in
  let data = Cairo.Image.get_data32 surface in
  let width = Cairo.Image.get_width surface in
  let height = Cairo.Image.get_height surface in
  let stride = Cairo.Image.get_stride surface in
  { data; width; height; stride }
;;

let load_camel obj_file =
  let vertices, normals, faces, textures, kd, ks, ka = read_obj_file obj_file in
  let triangles = convert_to_scene (vertices, normals, faces, textures, kd, ks, ka) in
  let rotation_mat =
    Transformations.rotation_transformation_of_axis ~angle:(5. *. Float.pi /. 4.) Y
  in
  let triangles = rotate_mesh triangles rotation_mat Y in
  (* let scene_min, scene_max = scene_limits triangles in *)
  (* let scene_center = Point.mean [ scene_max; scene_min ] |> Option.get in *)
  (* let triangles = List.map (scale_figure 0.5 0.5 0.5 scene_center) triangles in *)
  let real_scene = split_scene triangles LargestAxis in
  (* let real_scene = translate_figure (-2.5) (-4.5) (-0.5) (List.nth real_scene 0) in *)
  let real_scene = translate_figure 0.8 (-1.8) (-0.5) (List.nth real_scene 0) in
  [ real_scene ] @ my_scene
;;

let () =
  Random.self_init ();
  let camera = camera !up !left !forward !origin (!width, !height) in
  let oc = open_out "ppms/rendered/cornell.ppm" in
  let out_conf : PpmDb.config = PpmDb.config_of_values "P3" 1. 255 !width !height in
  PpmDb.write_header oc out_conf;
  let my_scene = load_camel "obj_files/camel2.obj" in
  let texture_map = texture_map_from_file "textures/camel2.png" in
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
