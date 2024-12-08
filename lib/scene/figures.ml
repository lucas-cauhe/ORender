(*
   Authors: Lucas Cauhé Viñao, Andrei Dumbrava
   Description: Figures interface implementation
*)

open Geometry
open Colorspace

let eps = ref 10e-5

type coefficients = Colorspace.Rgb.pixel * Colorspace.Rgb.pixel * Colorspace.Rgb.pixel

type ray_type =
  { ray_origin : Point.point_t
  ; ray_direction : Direction.direction_t
  }

let ray o d = { ray_origin = o; ray_direction = d }

let string_of_ray ray =
  Printf.sprintf
    "Ray dir -> %s, Ray origin -> %s"
    (Direction.string_of_direction ray.ray_direction)
    (Point.string_of_point ray.ray_origin)
;;

type plane_type =
  { plane_normal : Direction.direction_t
  ; plane_origin : Point.point_t
  }

type sphere_type =
  { sphere_radius : float
  ; sphere_center : Point.point_t
  }

type triangle_type =
  { vert_a : Point.point_t
  ; vert_b : Point.point_t
  ; vert_c : Point.point_t
  ; triangle_normal : Direction.direction_t
  }

type cuboid_type =
  { cuboid_min : Point.point_t
  ; cuboid_max : Point.point_t
  }

type figure_type =
  | Empty
  | Plane of plane_type
  | Sphere of sphere_type
  | Triangle of triangle_type
  | Cuboid of cuboid_type
(* | Cylinder of cylinder_type *)

type figure_properties =
  { emission : Colorspace.Rgb.pixel
  ; coefficients : coefficients
  ; refraction : float
  }

type figure =
  { fig_type : figure_type
  ; fig_properties : figure_properties
  }

type scene_figure =
  | Figure of figure
  | BoundingBox of figure * scene_figure list

type scene = scene_figure list

type intersection =
  { distance : float
  ; surface_normal : Direction.direction_t
  ; intersection_point : Point.point_t
  }

type intersection_result =
  | Zero
  | Intersects of intersection list

let get_figure = function
  | Figure fig -> fig
  | BoundingBox (fig, _) -> fig
;;

let scene_size scene = List.length scene

let point_of_ray ray dist =
  let origin_dir = Direction.of_point ray.ray_origin in
  let dir_sum = Direction.prod ray.ray_direction dist |> Direction.sum origin_dir in
  Point.from_coords (Direction.x dir_sum) (Direction.y dir_sum) (Direction.z dir_sum)
;;

let dist_to_point_of_ray ray point =
  Point.x point -. Point.x ray.ray_origin |> ( *. ) (1. /. Direction.x ray.ray_direction)
;;

let emission fig = fig.fig_properties.emission
let coefficients fig = fig.fig_properties.coefficients
let refraction fig = fig.fig_properties.refraction

(******************************)
(* PLANE ASSOCIATED FUNCTIONS *)
(******************************)

let plane d o props =
  { fig_type =
      Plane { plane_normal = Direction.normalize d |> Option.get; plane_origin = o }
  ; fig_properties = props
  }
;;

(* Plane implicit equation: ax + by + cz + d = 0 *)
(* Ray implicit equation: p + d * t = 0 *)
let plane_intersection (plane : plane_type) (ray : ray_type) : intersection_result =
  let open Direction in
  (* Check if ray and plane are parallel *)
  match plane.plane_normal * ray.ray_direction with
  | 0. -> Zero
  | den ->
    let c = of_point plane.plane_origin * plane.plane_normal in
    let num = of_point ray.ray_origin * plane.plane_normal |> ( +. ) (-.c) in
    (match -.num /. den with
     | neg when neg <= 10e-5 -> Zero
     | pos ->
       Intersects
         [ { distance = pos
           ; surface_normal = plane.plane_normal
           ; intersection_point = point_of_ray ray pos
           }
         ])
;;

let show_plane (plane : plane_type) =
  Printf.printf
    "PLANE {Normal: %s, Origin: %s}\n"
    (Direction.string_of_direction plane.plane_normal)
    (Point.string_of_point plane.plane_origin)
;;

let transform_plane (_ : transformation) (fig : plane_type) =
  let complete_transform props = Some (plane fig.plane_normal fig.plane_origin props) in
  complete_transform
;;

let point_belongs_to_plane (p : Point.point_t) (fig : plane_type) =
  Direction.between_points p fig.plane_origin
  |> Direction.dot fig.plane_normal
  |> abs_float
  < !eps
;;

(*******************************)
(* SPHERE ASSOCIATED FUNCTIONS *)
(*******************************)

let sphere center radius props =
  { fig_type = Sphere { sphere_center = center; sphere_radius = radius }
  ; fig_properties = props
  }
;;

let sphere_intersection (sphere : sphere_type) (ray : ray_type) : intersection_result =
  let surface_normal d =
    Direction.between_points (point_of_ray ray d) sphere.sphere_center
    |> Direction.normalize
    |> Option.get
  in
  let oc = Direction.between_points ray.ray_origin sphere.sphere_center in
  let a = Direction.modulus ray.ray_direction |> Common.square in
  let b = 2.0 *. Direction.dot oc ray.ray_direction in
  let c =
    (Point.distance ray.ray_origin sphere.sphere_center |> Common.square)
    -. Common.square sphere.sphere_radius
  in
  let discriminant = (b *. b) -. (4.0 *. a *. c) in
  if abs_float discriminant <= 1e-10 then (
    let dist = -.b /. (2. *. a) in
    let snormal = surface_normal dist in
    Intersects
      [ { distance = dist
        ; surface_normal = snormal
        ; intersection_point = point_of_ray ray dist
        }
      ]
  ) else if discriminant > 0. then (
    let t1 = (-.b -. sqrt discriminant) /. (2.0 *. a) in
    let t2 = (-.b +. sqrt discriminant) /. (2.0 *. a) in
    let fst =
      if t1 >= 10e-5 then
        [ { distance = t1
          ; surface_normal = surface_normal t1
          ; intersection_point = point_of_ray ray t1
          }
        ]
      else
        []
    in
    let snd =
      if t2 > 0. then
        [ { distance = t2
          ; surface_normal = surface_normal t2
          ; intersection_point = point_of_ray ray t2
          }
        ]
      else
        []
    in
    match fst, snd with
    | [], [] -> Zero
    | f, s -> Intersects (f @ s)
  ) else
    Zero
;;

let show_sphere (sphere : sphere_type) =
  Printf.printf
    "SPHERE {Center: %s, Radius: %f}\n"
    (Point.string_of_point sphere.sphere_center)
    sphere.sphere_radius
;;

let transform_sphere (t : transformation) (fig : sphere_type)
  : figure_properties -> figure option
  =
  match t with
  | Translation (tx, ty, tz) ->
    let translation_mat = Transformations.translation_transformation_of_values tx ty tz in
    let complete_transform props =
      Option.bind
        (Transformations.hc_of_point fig.sphere_center
         |> Transformations.translate translation_mat)
        (fun hc ->
          let translated_point = Transformations.point_of_hc hc in
          Some (sphere translated_point fig.sphere_radius props))
    in
    complete_transform
  | _ ->
    let complete_transform props =
      Some (sphere fig.sphere_center fig.sphere_radius props)
    in
    complete_transform
;;

let sphere_vertices (sphere : sphere_type) : Point.point_t list =
  let x, y, z =
    ( Point.x sphere.sphere_center
    , Point.y sphere.sphere_center
    , Point.z sphere.sphere_center )
  in
  [ Point.from_coords (x +. sphere.sphere_radius) y z
  ; Point.from_coords (x -. sphere.sphere_radius) y z
  ; Point.from_coords x (y +. sphere.sphere_radius) z
  ; Point.from_coords x (y -. sphere.sphere_radius) z
  ; Point.from_coords x y (z +. sphere.sphere_radius)
  ; Point.from_coords x y (z -. sphere.sphere_radius)
  ]
;;

let point_belongs_to_sphere (p : Point.point_t) (fig : sphere_type) =
  let xsquare = Point.x p |> Common.square in
  let ysquare = Point.y p |> Common.square in
  let zsquare = Point.z p |> Common.square in
  xsquare +. ysquare +. zsquare -. (fig.sphere_radius |> Common.square)
  |> abs_float
  < !eps
;;

(*********************************)
(* TRIANGLE ASSOCIATED FUNCTIONS *)
(*********************************)

let triangle a b c props =
  match
    Direction.cross_product (Direction.between_points b a) (Direction.between_points c a)
    |> Direction.normalize
  with
  | Some normal ->
    Some
      { fig_type =
          Triangle { vert_a = a; vert_b = b; vert_c = c; triangle_normal = normal }
      ; fig_properties = props
      }
  | None -> None
;;

let triangle_intersection (triangle : triangle_type) (ray : ray_type) =
  let open Direction in
  match
    plane_intersection
      { plane_normal = triangle.triangle_normal; plane_origin = triangle.vert_a }
      ray
  with
  | Intersects [ { distance = d; _ } ] as il ->
    let p = point_of_ray ray d in
    let v0 = between_points triangle.vert_b triangle.vert_a in
    let v1 = between_points triangle.vert_c triangle.vert_b in
    let v2 = between_points triangle.vert_a triangle.vert_c in
    let pa = between_points p triangle.vert_a in
    let pb = between_points p triangle.vert_b in
    let pc = between_points p triangle.vert_c in
    let left_a = cross_product v0 pa |> dot triangle.triangle_normal in
    let left_b = cross_product v1 pb |> dot triangle.triangle_normal in
    let left_c = cross_product v2 pc |> dot triangle.triangle_normal in
    if left_a >= 0. && left_b >= 0. && left_c >= 0. then
      il
    else
      Zero
  | _ -> Zero
;;

let show_triangle (triangle : triangle_type) =
  Printf.printf
    "TRIANGLE {A: %s, B: %s, C: %s, Normal: %s}\n"
    (Point.string_of_point triangle.vert_a)
    (Point.string_of_point triangle.vert_b)
    (Point.string_of_point triangle.vert_c)
    (Direction.string_of_direction triangle.triangle_normal)
;;

let transform_triangle (transform : transformation) (t : triangle_type)
  : figure_properties -> figure option
  =
  match transform with
  | Translation (tx, ty, tz) ->
    let translation_mat = Transformations.translation_transformation_of_values tx ty tz in
    let translated_points =
      Array.map
        (fun point ->
          Transformations.hc_of_point point
          |> Transformations.translate translation_mat
          |> Option.get
          |> Transformations.point_of_hc)
        [| t.vert_a; t.vert_b; t.vert_c |]
    in
    triangle translated_points.(0) translated_points.(1) translated_points.(2)
  | Scale (sx, sy, sz) ->
    let scale_mat = Transformations.scale_transformation_of_values sx sy sz in
    let scaled_dirs =
      Array.map
        (fun dir ->
          Transformations.hc_of_direction dir
          |> Transformations.scale scale_mat
          |> Transformations.direction_of_hc)
        [| Direction.between_points t.vert_c t.vert_a
         ; Direction.between_points t.vert_b t.vert_a
        |]
    in
    triangle
      t.vert_a
      (Point.sum
         t.vert_a
         (Point.from_coords
            (Direction.x scaled_dirs.(0))
            (Direction.y scaled_dirs.(0))
            (Direction.z scaled_dirs.(0))))
      (Point.sum
         t.vert_a
         (Point.from_coords
            (Direction.x scaled_dirs.(1))
            (Direction.y scaled_dirs.(1))
            (Direction.z scaled_dirs.(1))))
  | Rotation (m, ax) ->
    let rotated_points =
      Array.map
        (fun point ->
          Transformations.hc_of_point point
          |> Transformations.rotate m ax
          |> Transformations.point_of_hc)
        [| t.vert_a; t.vert_b; t.vert_c |]
    in
    triangle rotated_points.(0) rotated_points.(1) rotated_points.(2)
  | _ -> triangle t.vert_a t.vert_b t.vert_c
;;

let triangle_vertices (triangle : triangle_type) =
  [ triangle.vert_a; triangle.vert_b; triangle.vert_c ]
;;

let triangle_barycenter (triangle : triangle_type) =
  Point.mean [ triangle.vert_a; triangle.vert_b; triangle.vert_c ] |> Option.get
;;

(*******************************)
(* CUBOID ASSOCIATED FUNCTIONS *)
(*******************************)

let cuboid c_min c_max props =
  { fig_type = Cuboid { cuboid_min = c_min; cuboid_max = c_max }; fig_properties = props }
;;

let cuboid_intersection (c : cuboid_type) (ray : ray_type) : intersection_result =
  let t1x =
    (Point.x c.cuboid_min -. Point.x ray.ray_origin) /. Direction.x ray.ray_direction
  in
  let t2x =
    (Point.x c.cuboid_max -. Point.x ray.ray_origin) /. Direction.x ray.ray_direction
  in
  let t1y =
    (Point.y c.cuboid_min -. Point.y ray.ray_origin) /. Direction.y ray.ray_direction
  in
  let t2y =
    (Point.y c.cuboid_max -. Point.y ray.ray_origin) /. Direction.y ray.ray_direction
  in
  let t1z =
    (Point.z c.cuboid_min -. Point.z ray.ray_origin) /. Direction.z ray.ray_direction
  in
  let t2z =
    (Point.z c.cuboid_max -. Point.z ray.ray_origin) /. Direction.z ray.ray_direction
  in
  let tmin = max (min t1x t2x) (min t1y t2y) |> max (min t1z t2z) in
  let tmax = min (max t1x t2x) (max t1y t2y) |> min (max t1z t2z) in
  if tmin < tmax && tmax > 0. then (
    let distance =
      if tmin > 0. then
        tmin
      else
        tmax
    in
    let normal =
      if distance = t1x || distance = t2x then
        Direction.from_coords
          (if distance = t1x then
             -1.
           else
             1.)
          0.
          0.
      else if distance = t1y || distance = t2y then
        Direction.from_coords
          0.
          (if distance = t1y then
             -1.
           else
             1.)
          0.
      else
        Direction.from_coords
          0.
          0.
          (if distance = t1z then
             -1.
           else
             1.)
    in
    Intersects
      [ { distance
        ; surface_normal = normal
        ; intersection_point = point_of_ray ray distance
        }
      ]
  ) else
    Zero
;;

let show_cuboid (cuboid : cuboid_type) =
  Printf.printf
    "min = %s; max = %s"
    (Point.string_of_point cuboid.cuboid_min)
    (Point.string_of_point cuboid.cuboid_max)
;;

let cuboid_vertices (cuboid : cuboid_type) = [ cuboid.cuboid_min; cuboid.cuboid_max ]

let cuboid_barycenter (cuboid : cuboid_type) =
  Point.mean [ cuboid.cuboid_max; cuboid.cuboid_min ] |> Option.get
;;

let point_belongs_to_cuboid (p : Point.point_t) (cuboid : cuboid_type) =
  let px, cubxmin, cubxmax =
    Point.x p, Point.x cuboid.cuboid_min, Point.x cuboid.cuboid_max
  in
  let py, cubymin, cubymax =
    Point.y p, Point.y cuboid.cuboid_min, Point.y cuboid.cuboid_max
  in
  let pz, cubzmin, cubzmax =
    Point.z p, Point.z cuboid.cuboid_min, Point.z cuboid.cuboid_max
  in
  px >= cubxmin
  && px <= cubxmax
  && py >= cubymin
  && py <= cubymax
  && pz >= cubzmin
  && pz <= cubzmax
;;

let transform_cuboid (transform : transformation) (figure : cuboid_type)
  : figure_properties -> figure option
  =
  let complete_transform fig props = Some (cuboid fig.cuboid_min fig.cuboid_max props) in
  match transform with
  | Translation (tx, ty, tz) ->
    let translation_mat = Transformations.translation_transformation_of_values tx ty tz in
    let translated_points =
      Array.map
        (fun point ->
          Transformations.hc_of_point point
          |> Transformations.translate translation_mat
          |> Option.get
          |> Transformations.point_of_hc)
        [| figure.cuboid_min; figure.cuboid_max |]
    in
    complete_transform
      { cuboid_min = translated_points.(0); cuboid_max = translated_points.(1) }
  | Rotation (m, ax) ->
    let rotated_points =
      Array.map
        (fun point ->
          Transformations.hc_of_point point
          |> Transformations.rotate m ax
          |> Transformations.point_of_hc)
        [| figure.cuboid_min; figure.cuboid_max |]
    in
    complete_transform
      { cuboid_min = rotated_points.(0); cuboid_max = rotated_points.(1) }
  | _ -> complete_transform figure
;;

(**************************)
(* BOUNDING BOX FUNCTIONS *)
(**************************)

let bounding_box cuboid children =
  match cuboid.fig_type with
  | Cuboid _ -> Some (BoundingBox (cuboid, children))
  | _ -> None
;;

(*************************)
(* INTERSECTION FUNCTION *)
(*************************)

let intersects fig ray =
  match fig.fig_type with
  | Plane plane -> plane_intersection plane ray
  | Sphere sphere -> sphere_intersection sphere ray
  | Triangle triangle -> triangle_intersection triangle ray
  | Cuboid cuboid -> cuboid_intersection cuboid ray
  | Empty -> Zero
;;

(*************************)
(*     SHOW FUNCTION     *)
(*************************)

let show_figure fig =
  match fig.fig_type with
  | Plane plane -> show_plane plane
  | Sphere sphere -> show_sphere sphere
  | Triangle triangle -> show_triangle triangle
  | Cuboid cuboid -> show_cuboid cuboid
  | Empty -> print_endline "Empty figure"
;;

(*****************************)
(*    TRANSFORM FUNCTION     *)
(*****************************)
let transform t fig =
  match fig.fig_type with
  | Empty -> None
  | Plane plane -> transform_plane t plane fig.fig_properties
  | Sphere sphere -> transform_sphere t sphere fig.fig_properties
  | Triangle triangle -> transform_triangle t triangle fig.fig_properties
  | Cuboid cuboid -> transform_cuboid t cuboid fig.fig_properties
;;

(****************************)
(*    VERTICES FUNCTION     *)
(****************************)
let vertices fig =
  match fig.fig_type with
  | Plane _ | Empty -> []
  | Sphere sphere -> sphere_vertices sphere
  | Triangle triangle -> triangle_vertices triangle
  | Cuboid cuboid -> cuboid_vertices cuboid
;;

(******************************)
(*    BARYCENTER FUNCTION     *)
(******************************)
let barycenter fig =
  match fig.fig_type with
  | Plane plane -> plane.plane_origin
  | Sphere sphere -> sphere.sphere_center
  | Triangle triangle -> triangle_barycenter triangle
  | Cuboid cuboid -> cuboid_barycenter cuboid
  | Empty -> Point.from_coords 0. 0. 0.
;;

let point_belongs_to_fig p fig =
  match fig.fig_type with
  | Plane plane -> point_belongs_to_plane p plane
  | Sphere sphere -> point_belongs_to_sphere p sphere
  | Triangle _ -> false (* not yet implemented *)
  | Cuboid cuboid -> point_belongs_to_cuboid p cuboid
  | Empty -> false
;;

let empty () =
  { fig_type = Empty
  ; fig_properties =
      { emission = Rgb.zero ()
      ; coefficients = Rgb.zero (), Rgb.zero (), Rgb.zero ()
      ; refraction = 0.
      }
  }
;;

(** Returns [None] if ray doesn't intersect any figure in the scene. Otherwise return the first figure in the scene that the ray intersects with
    wrapped in [Some]
    As it is implemented, thread safety is guaranteed since intersections are stored in an array at their figure's scene position
    Rather than workin with locks for computing the minimum on the fly we've decided to perform a min operation over the intersections array later. *)
let rec find_closest_figure scene ray =
  let min (min_set : scene_figure * intersection_result) fig next =
    match min_set, next with
    | _, Zero -> min_set
    | (_, Zero), Intersects _ -> fig, next
    | (_, Intersects dist_min), Intersects curr ->
      if (List.hd dist_min).distance < (List.hd curr).distance then
        min_set
      else
        fig, next
  in
  let rec loop_ closest figures =
    match figures with
    | [] -> closest
    | fig :: rest ->
      (match fig with
       | Figure f -> loop_ (min closest fig (intersects f ray)) rest
       | BoundingBox (f, rest_figures) ->
         (match intersects f ray with
          | Zero -> loop_ closest rest
          | Intersects _ ->
            (match find_closest_figure rest_figures ray with
             | None -> loop_ closest rest
             | Some (inner_figure, _) ->
               loop_
                 (min closest inner_figure (intersects (get_figure inner_figure) ray))
                 rest)))
  in
  match loop_ (List.hd scene, Zero) scene with
  | _, Zero -> None
  | fig, (Intersects _ as inter) -> Some (fig, inter)
;;

let is_sphere = function
  | Figure { fig_type = Sphere _; _ } -> true
  | _ -> false
;;

let is_plane = function
  | Figure { fig_type = Plane _; _ } -> true
  | _ -> false
;;

let is_same_plane
  ({ plane_normal = pn1; _ } as plane1)
  { plane_normal = pn2; plane_origin = po2 }
  =
  (* normals are equal and either point belongs to the other plane *)
  Direction.eq pn1 pn2 && point_belongs_to_plane po2 plane1
;;

let is_same_sphere
  { sphere_radius = sr1; sphere_center = sc1 }
  { sphere_radius = sr2; sphere_center = sc2 }
  =
  (* same radius and center point *)
  sr1 = sr2 && Point.eq sc1 sc2
;;

let is_same_triangle
  { vert_a = va1; vert_b = vb1; vert_c = vc1; _ }
  { vert_a = va2; vert_b = vb2; vert_c = vc2; _ }
  =
  (* three vertices are the same & in the same order *)
  Point.eq va1 va2 && Point.eq vb1 vb2 && Point.eq vc1 vc2
;;

let is_same_figure fig1 fig2 =
  match fig1.fig_type, fig2.fig_type with
  | Empty, Empty -> true
  | Plane plane1, Plane plane2 -> is_same_plane plane1 plane2
  | Sphere sphere1, Sphere sphere2 -> is_same_sphere sphere1 sphere2
  | Triangle triangle1, Triangle triangle2 -> is_same_triangle triangle1 triangle2
  | _, _ -> false
;;

let rotate_figure scene_fig rotation_matrix axis =
  let translate_fig central fig =
    transform
      (Geometry.Translation (-.Point.x central, -.Point.y central, -.Point.z central))
      fig
    |> Option.get
  in
  let rec rotate_internal central root figs =
    let rotated_figs = List.map (rotate_one central) figs in
    BoundingBox
      ( translate_fig central root
        |> transform (Geometry.Rotation (rotation_matrix, axis))
        |> Option.get
      , rotated_figs )
  and rotate_one center = function
    | Figure fig ->
      Figure
        (transform (Geometry.Rotation (rotation_matrix, axis)) (translate_fig center fig)
         |> Option.get)
    | BoundingBox (box, next_figs) -> rotate_internal center box next_figs
  in
  match scene_fig with
  | Figure fig ->
    Figure (transform (Geometry.Rotation (rotation_matrix, axis)) fig |> Option.get)
  | BoundingBox (({ fig_type = Cuboid cuboid; _ } as bounding_volume), figs) ->
    rotate_internal (cuboid_barycenter cuboid) bounding_volume figs
  | _ -> scene_fig
;;

let rec translate_figure x y z = function
  | Figure fig -> Figure (transform (Geometry.Translation (x, y, z)) fig |> Option.get)
  | BoundingBox (box, next_figs) ->
    let translated_figs = List.map (translate_figure x y z) next_figs in
    let translated_box = transform (Geometry.Translation (x, y, z)) box |> Option.get in
    BoundingBox (translated_box, translated_figs)
;;
