module Photon : sig
include Kdtree.KdTreePoint
 
  type t
  val ndims : int
  val get_coord : int -> t -> float
  val create : float array -> t
  
  val photon : Colorspace.Rgb.pixel -> Geometry.Point.point_t -> Geometry.Direction.direction_t -> t
  val position : t -> Geometry.Point.point_t
  val direction : t -> Geometry.Direction.direction_t
  val flux : t -> Colorspace.Rgb.pixel
end