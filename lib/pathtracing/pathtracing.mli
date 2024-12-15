module Bindings = Bindings

(** Implements the path tracing algorithm *)
val path_tracing : Scene.Figures.scene -> Scene.Light.light_source list -> Scene.Figures.ray_type -> Colorspace.Rgb.pixel

(** Traces a ray [ray] accross the given [scene] to find which is the first figure that intersects with the ray, if any *)
val trace_ray : Scene.Figures.scene -> Scene.Figures.ray_type -> Scene.Figures.scene_figure * Scene.Figures.intersection_result

(** Compute the direct light given a [Light.light_source] and a [Figures.intersection].
    Returns a function that expects the brdf value to finally compute value of the direct light *)
val direct_light : Scene.Figures.scene -> Scene.Light.light_source list -> Scene.Figures.intersection -> Colorspace.Rgb.pixel -> Colorspace.Rgb.pixel