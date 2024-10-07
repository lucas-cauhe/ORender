type ray_type = {
  origin: Geometry.Point.t;
  direction: Geometry.Direction.t
}

module type FigureSig = sig
    type config

    (** Dada una figura y un rayo, devuelve una lista con las distancias a los puntos con los que intersecta
        el rayo en la figura
    *)
    val intersects : config -> ray_type -> float list
    val show : config -> unit

    
end

module Plane : sig
  include FigureSig
  val init : Geometry.Direction.t -> Geometry.Point.t -> config
end
module Sphere : sig
    include FigureSig
    val init : Geometry.Point.t -> float -> config
    val center : config -> Geometry.Point.t
    val radius : config -> float
end

type figure = Plane of Plane.config | Sphere of Sphere.config
type scene = figure list

val find_closest_figure : scene -> ray_type -> figure
val show_figure : figure -> unit
