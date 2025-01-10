open Geometry
open Colorspace
open Scene

type bvh_algorithm =
  | LargestAxis
  | Sah

let bvh_primitives_minimum = 4

(*********************************************)
(* SPLIT SCENE ACROSS LARGEST AXIS ALGORITHM *)
(*********************************************)

let scene_limits (scene : Figures.scene) : Point.point_t * Point.point_t =
  let min_max_point (min_point, max_point) point =
    ( Point.from_coords
        (min (Point.x min_point) (Point.x point))
        (min (Point.y min_point) (Point.y point))
        (min (Point.z min_point) (Point.z point))
    , Point.from_coords
        (max (Point.x max_point) (Point.x point))
        (max (Point.y max_point) (Point.y point))
        (max (Point.z max_point) (Point.z point)) )
  in
  let min_max_figure min max fig =
    List.fold_left min_max_point (min, max) (Figures.vertices fig)
  in
  let rec min_max_scene figures min_point max_point =
    match figures with
    | [] -> min_point, max_point
    | fig :: rest ->
      let min, max = min_max_figure min_point max_point (Figures.get_figure fig) in
      min_max_scene rest min max
  in
  let initial_min = Point.from_coords Float.infinity Float.infinity Float.infinity in
  let initial_max =
    Point.from_coords (-.Float.infinity) (-.Float.infinity) (-.Float.infinity)
  in
  min_max_scene scene initial_min initial_max
;;

let largest_axis
  (lower_bound : Point.point_t)
  (upper_bound : Point.point_t)
  (scene : Figures.scene)
  : axis * float
  =
  let x_dist = Point.x upper_bound -. Point.x lower_bound in
  let y_dist = Point.y upper_bound -. Point.y lower_bound in
  let z_dist = Point.z upper_bound -. Point.z lower_bound in
  let barycenters =
    List.fold_left
      (fun acc fig -> Point.sum acc (Figures.barycenter (Figures.get_figure fig)))
      (Point.from_coords 0. 0. 0.)
      scene
  in
  if abs_float x_dist >= abs_float y_dist && abs_float x_dist >= abs_float z_dist then
    X, Point.x barycenters /. (Figures.scene_size scene |> float_of_int)
  else if abs_float y_dist > abs_float x_dist && abs_float y_dist >= abs_float z_dist then
    Y, Point.y barycenters /. (Figures.scene_size scene |> float_of_int)
  else
    Z, Point.z barycenters /. (Figures.scene_size scene |> float_of_int)
;;

let split_scene_by_axis (scene : Figures.scene) (a : axis) (mid_point : float)
  : Figures.scene * Figures.scene
  =
  let rec loop_ scene_a scene_b rest_figs =
    match rest_figs with
    | [] -> scene_a, scene_b
    | fig :: rest ->
      let barycenter = Figures.barycenter (Figures.get_figure fig) in
      if value_at_axis a barycenter <= mid_point then
        loop_ (fig :: scene_a) scene_b rest
      else
        loop_ scene_a (fig :: scene_b) rest
  in
  loop_ [] [] scene
;;

let cuboid_center (scene : Figures.scene) : Point.point_t =
  let scene_min, scene_max = scene_limits scene in
  let scene_bbox =
    Figures.cuboid
      scene_min
      scene_max
      { emission = Rgb.zero ()
      ; coefficients = Rgb.zero (), Rgb.zero (), Rgb.zero ()
      ; refraction = 0.
      }
  in
  Figures.barycenter scene_bbox
;;

let rec split_largest_axis (scene : Figures.scene) : Figures.scene =
  let scene_min, scene_max = scene_limits scene in
  let scene_bbox =
    Figures.cuboid
      scene_min
      scene_max
      { emission = Rgb.zero ()
      ; coefficients = Rgb.zero (), Rgb.zero (), Rgb.zero ()
      ; refraction = 0.
      }
  in
  match Figures.scene_size scene with
  | s when s <= bvh_primitives_minimum ->
    [ Figures.bounding_box scene_bbox scene |> Option.get ]
  | _ ->
    let l_axis, mid_point = largest_axis scene_min scene_max scene in
    let scene_a, scene_b = split_scene_by_axis scene l_axis mid_point in
    [ Figures.bounding_box
        scene_bbox
        (split_largest_axis scene_a @ split_largest_axis scene_b)
      |> Option.get
    ]
;;

(*****************)
(* SAH ALGORITHM *)
(*****************)

let split_sah (_scene : Figures.scene) : Figures.scene = []

(**********************)
(* MODULE'S INTERFACE *)
(**********************)

let split_scene scene algo =
  let scene_wout_planes = List.filter (fun fig -> not @@ Figures.is_plane fig) scene in
  let new_scene_wout_planes =
    match algo with
    | LargestAxis -> split_largest_axis scene_wout_planes
    | Sah -> split_sah scene_wout_planes
  in
  List.filter Figures.is_plane scene @ new_scene_wout_planes
;;
