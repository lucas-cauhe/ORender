module Photon : sig
include Kdtree.Multidim
  type t
  val photon : Colorspace.Rgb.pixel -> Geometry.Point.point_t -> Geometry.Direction.direction_t ->  t
  val position : t -> Geometry.Point.point_t
  val direction : t -> Geometry.Direction.direction_t
  val flux : t -> Colorspace.Rgb.pixel
  val to_string : t -> string
end