open Geometry

type gaussian_kernel =
  { intersection_position : Point.point_t
  ; smooth : float
  }

type epanechnikov_kernel =
  { intersection_position : Point.point_t
  ; scale_parameter : float
  }

type tricube_kernel =
  { intersection_position : Point.point_t
  ; bandwidth : float
  }

type kernel_type =
  | Box of float
  | Gaussian of gaussian_kernel
  | Epanechnikov of epanechnikov_kernel
  | Tricube of tricube_kernel

let build_box = Box 1.

let build_gaussian =
  Gaussian { intersection_position = Point.from_coords 0. 0. 0.; smooth = 0.5 }
;;

let build_epanechnikov =
  Epanechnikov
    { intersection_position = Point.from_coords 0. 0. 0.; scale_parameter = sqrt 5. }
;;

let build_tricube =
  Tricube { intersection_position = Point.from_coords 0. 0. 0.; bandwidth = 0.5 }
;;

let kernel_fun (photon : Photon.t) = function
  | Box radius -> 1. /. Float.pi /. Common.square radius
  | Gaussian { intersection_position; smooth } ->
    (Float.exp
     @@ -.Common.square
            ((Photon.direction_to_point intersection_position photon |> Direction.modulus)
             /. smooth))
    /. Float.pi
    /. Common.square smooth
  | Epanechnikov { intersection_position; scale_parameter } ->
    let r = Photon.direction_to_point intersection_position photon |> Direction.modulus in
    let u = r /. scale_parameter in
    3. /. (4. *. Float.pi *. scale_parameter *. scale_parameter) *. (1. -. (u *. u))
  | Tricube { intersection_position; bandwidth } ->
    let r = Photon.direction_to_point intersection_position photon |> Direction.modulus in
    let t = abs_float (r /. bandwidth) in
    let cube = 1. -. (t *. t *. t) in
    70. *. cube *. cube *. cube /. (81. *. Float.pi *. bandwidth *. bandwidth)
;;
