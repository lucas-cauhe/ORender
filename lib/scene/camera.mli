type camera

val camera :
  Geometry.Direction.direction_t ->
  Geometry.Direction.direction_t ->
  Geometry.Direction.direction_t ->
  Geometry.Point.point_t ->
  int * int ->
  camera

val points_in_pixel :
  camera -> int * int -> int -> Geometry.Point.point_t BatList.t
(**
  Returns the defined number of points inside a pixel to trace a ray through.
*)

val cam_origin : camera -> Geometry.Point.point_t
