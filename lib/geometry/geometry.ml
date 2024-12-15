module Point = Point
module Direction = Direction
module Matrix = Matrix
module Transformations = Transformations

type axis = Transformations.axis

let value_at_axis (a : axis) p =
  match a with
  | X -> Point.x p
  | Y -> Point.y p
  | Z -> Point.z p
;;

type transformation =
  | Rotation of Matrix.matrix_t * axis
  | Scale of float * float * float * Point.point_t
  | Translation of float * float * float
  | ChangeBase of Matrix.matrix_t

let cartesian_of_spherical (lat : float) (azimuth : float) (n : Direction.direction_t) =
  let r = Direction.modulus n in
  Direction.from_coords
    (r *. sin lat *. cos azimuth)
    (r *. sin lat *. sin azimuth)
    (r *. cos lat)
  |> Direction.normalize
  |> Option.get
;;

let cb_matrix_tangent (n : Direction.direction_t) (origin : Point.point_t) =
  (* let u = Direction.from_coords (-.r *. sin lat *. cos azimuth) (r *. sin lat *. sin azimuth) 0. in *)
  let n_perp = Direction.perpendicular n in
  let u = Direction.cross_product n_perp n in
  let v = Direction.cross_product u n in
  Transformations.cb_transformation_of_base u v n origin
;;
