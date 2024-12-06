open Colorspace
open Scene
open Geometry

(***************************)
(*    PHOTON DEFINITION    *)
(***************************)

type t =
  { flux : Rgb.pixel
  ; position : Point.point_t
  ; direction : Direction.direction_t
  ; surface : Figures.figure
  }

let to_ray photon = Figures.ray photon.position photon.direction
let photon flux position direction surface = { flux; position; direction; surface }
let flux ph = ph.flux

let to_string ph =
  Printf.sprintf
    "Flux -> %s | Position -> %s | Direction -> %s\n"
    (Rgb.show ph.flux)
    (Point.string_of_point ph.position)
    (Direction.string_of_direction ph.direction)
;;

let brdf fig surface_normal ray_direction roulette photon =
  Brdf.brdf fig surface_normal ray_direction photon.direction roulette
;;

let in_surface fig photon = Figures.is_same_figure fig photon.surface
let direction_to_point point photon = Direction.between_points point photon.position

(***************************)
(* MULTIDIM IMPLEMENTATION *)
(***************************)
let dim = 3
let point p = p

type real = float
type point = Point.point_t

let axial_compare n p1 p2 =
  match n with
  | 0 -> compare (Point.x p1) (Point.x p2)
  | 1 -> compare (Point.y p1) (Point.y p2)
  | 2 -> compare (Point.z p1) (Point.z p2)
  | _ -> invalid_arg @@ "Expected axis 0, 1 or 2, found " ^ string_of_int n
;;

let to_point { position = p; _ } = p
let squared_distance p1 p2 = Geometry.Point.distance p1 p2 |> Common.square

let squared_axial_distance n p1 p2 =
  match n with
  | 0 -> Point.x p1 -. Point.x p2 |> Common.square
  | 1 -> Point.y p1 -. Point.y p2 |> Common.square
  | 2 -> Point.z p1 -. Point.z p2 |> Common.square
  | _ -> invalid_arg @@ "Expected axis 0, 1 or 2, found " ^ string_of_int n
;;
