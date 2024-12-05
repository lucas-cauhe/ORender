open Geometry

type gaussian_kernel =
  { intersection_position : Point.point_t
  ; smooth : float
  }

type kernel_type =
  | Box of float
  | Gaussian of gaussian_kernel

let build_box = Box 1.

let build_gaussian =
  Gaussian { intersection_position = Point.from_coords 0. 0. 0.; smooth = 0.5 }
;;

let kernel_fun (photon : Photon.t) = function
  | Box radius -> 1. /. Float.pi /. radius
  | Gaussian { intersection_position; smooth } ->
    (Float.exp
     @@ -.Common.square
            ((Photon.direction_to_point intersection_position photon |> Direction.modulus)
             /. smooth))
    /. Float.pi
    /. Common.square smooth
;;
