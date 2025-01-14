open Domainslib
open Scene.Figures
open Geometry
open Colorspace
open Scene.Camera
open Photonmap
open Pathtracing
module PpmDb = Db.Ppm

type algorithm =
  | Photonmap
  | Pathtracing

type algorithm_internal =
  | Photonmap of PhotonMap.t
  | Pathtracing

let num_points = ref 1024

let bar ~total =
  let open Progress.Line in
  list [ spinner (); bar total; count_to total ]
;;

let photonmap_pixel_color cam (row, col) ls scene photons pool texture_map =
  let pip_arr = BatArray.of_list (points_in_pixel cam (row, col) !num_points) in
  let compute_pixel_color ind =
    Direction.between_points pip_arr.(ind) (cam_origin cam)
    |> Direction.normalize
    |> Option.get
    |> ray (cam_origin cam)
    |> photonmap scene ls photons texture_map
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

let pathtracing_pixel_color cam (row, col) scene light_sources pool texture_map =
  let pip_arr = BatArray.of_list (points_in_pixel cam (row, col) !num_points) in
  let compute_pixel_color ind =
    Direction.between_points pip_arr.(ind) (cam_origin cam)
    |> Direction.normalize
    |> Option.get
    |> ray (cam_origin cam)
    |> path_tracing scene light_sources texture_map
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

let rec color_pixel
  algo
  row
  col
  reporter
  camera
  oc
  out_conf
  my_scene
  texture_map
  light_sources
  pool
  width
  height
  =
  match row, col with
  | r, _ when r = height -> close_out oc
  | _, _ ->
    let color =
      match algo with
      | Pathtracing ->
        pathtracing_pixel_color camera (row, col) my_scene light_sources pool texture_map
      | Photonmap photons ->
        photonmap_pixel_color
          camera
          (row, col)
          light_sources
          my_scene
          photons
          pool
          texture_map
    in
    reporter 1;
    PpmDb.write_pixel oc out_conf color;
    if col >= width - 1 then (
      let () = output_string oc "\n" in
      color_pixel
        algo
        (row + 1)
        0
        reporter
        camera
        oc
        out_conf
        my_scene
        texture_map
        light_sources
        pool
        width
        height
    ) else
      color_pixel
        algo
        row
        (col + 1)
        reporter
        camera
        oc
        out_conf
        my_scene
        texture_map
        light_sources
        pool
        width
        height
;;

let color_image
  (algorithm : algorithm)
  camera
  oc
  out_conf
  my_scene
  texture_map
  light_sources
  width
  height
  =
  let pool = Task.setup_pool ~num_domains:15 () in
  let real_algo =
    match algorithm with
    | Pathtracing -> Pathtracing
    | Photonmap -> Photonmap (random_walk my_scene light_sources 100000 pool)
  in
  (* let _ = random_walk my_scene light_sources 100000 pool in *)
  Progress.with_reporter
    (bar ~total:(width * height))
    (fun f ->
      color_pixel
        real_algo
        0
        0
        f
        camera
        oc
        out_conf
        my_scene
        texture_map
        light_sources
        pool
        width
        height);
  Task.teardown_pool pool
;;
