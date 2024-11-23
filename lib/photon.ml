open Colorspace
open Geometry

module Photon = struct
  type t =
    { flux : Rgb.pixel
    ; position : Point.point_t
    ; direction : Direction.direction_t
    }

  let ndims = 3

  let get_coord coord photon =
    match coord with
    | 0 -> Point.x photon.position
    | 1 -> Point.y photon.position
    | 2 -> Point.z photon.position
    | _ -> 0.
  ;;

  let create coords =
    { flux = Rgb.zero ()
    ; position = Point.from_coords coords.(0) coords.(1) coords.(2)
    ; direction = Direction.from_coords 0. 0. 0.
    }
  ;;

  let photon flux position direction = { flux; position; direction }
  let position ph = ph.position
  let direction ph = ph.direction
  let flux ph = ph.flux

  let to_string ph =
    Printf.sprintf
      "Flux -> %s | Position -> %s | Direction -> %s\n"
      (Rgb.show ph.flux)
      (Point.string_of_point ph.position)
      (Direction.string_of_direction ph.direction)
  ;;
end
