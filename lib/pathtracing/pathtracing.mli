module Bindings = Bindings
val path_tracing : Scene.Figures.scene -> Scene.Light.light_source list -> Scene.Figures.ray_type -> Colorspace.Rgb.pixel
val trace_ray : Scene.Figures.scene -> Scene.Figures.ray_type -> Scene.Figures.scene_figure * Scene.Figures.intersection_result