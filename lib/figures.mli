module Ray : sig
  type t
end

module type Figure = sig
    type config

    (** Dada una figura y un rayo, devuelve la lista de puntos con los que intersecta
        el rayo en la figura
    *)
    val intersects : config -> Ray.t -> float list

    
end

module Plane : sig
  include Figure
  val init : Geometry.Direction.t -> Geometry.Point.t -> config
end
module Sphere : sig
    include Figure
    val init : Geometry.Point.t -> float -> config
    val center : config -> Geometry.Point.t
    val radius : config -> float
end