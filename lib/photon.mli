module Photon : sig
include Kdtree.Multidim
  val photon : Colorspace.Rgb.pixel -> Geometry.Point.point_t -> Geometry.Direction.direction_t -> Scene.Figures.figure ->  t
  val position : t -> Geometry.Point.point_t
  val direction : t -> Geometry.Direction.direction_t
  val flux : t -> Colorspace.Rgb.pixel
  val surface : t -> Scene.Figures.figure
  val to_string : t -> string
  val point : Geometry.Point.point_t -> point
end