open Geometry
open Colorspace
open Figures

type light_source_type =
  | Area of Figures.scene_figure
  | Point of Geometry.Point.point_t

type light_source =
  { ls_power : Rgb.pixel
  ; ls_type : light_source_type
  }

let light_source t p = { ls_power = p; ls_type = t }
let light_source_type_val ls = ls.ls_type
let power ls = ls.ls_power

let shadow_ray scene intersection ls =
  let ls_center =
    match ls.ls_type with
    | Area _ ->
      Point.from_coords
        0.
        0.
        0. (* This function should never be called with area light *)
    | Point p -> p
  in
  match
    Direction.between_points ls_center intersection.intersection_point
    |> Direction.normalize
  with
  | None -> ls.ls_power
  | Some center_to_point ->
    let ray_to_ls = ray intersection.intersection_point center_to_point in
    (match Figures.find_closest_figure scene ray_to_ls with
     | Some (_, Intersects ({ distance = dist; _ } :: _))
       when dist < dist_to_point_of_ray ray_to_ls ls_center -> Rgb.zero ()
     | _ -> Rgb.normalize ls.ls_power (Direction.modulus center_to_point |> Common.square))
;;

let point_belongs_to_ls p ls =
  match ls.ls_type with
  | Area fig -> Figures.point_belongs_to_fig p (Figures.get_figure fig)
  | Point _ -> false
;;

let sample_light ls =
  match (List.hd ls).ls_type with
  | Point _ -> ls
  | Area _ -> [ List.nth ls (Random.int (List.length ls)) ]
;;

let sample_light_point ls =
  match ls.ls_type with
  | Point p -> p
  | Area _ -> Point.from_coords 0. 0. 0.
;;

let sample_light_source ls =
  match ls.ls_type with
  | Point _ -> ls
  | Area _ -> { ls with ls_type = Point (sample_light_point ls) }
;;

let to_string ls =
  match ls.ls_type with
  | Point p -> Printf.sprintf "Point Light -> %s" (Point.string_of_point p)
  | Area _ -> Printf.sprintf "Area Light"
;;
