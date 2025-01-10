val ( let* ) : Brdf.russian_roulette_result * float -> (Brdf.russian_roulette_result * float -> Colorspace.Rgb.pixel) -> Colorspace.Rgb.pixel
(** Binding for declaring russian roulette result and probability handling the case of absorption *)

val ( let& ) : (Scene.Figures.scene_figure * Scene.Figures.intersection_result) * Scene.Light.light_source list -> (Scene.Figures.scene_figure * Scene.Figures.intersection -> Colorspace.Rgb.pixel) -> Colorspace.Rgb.pixel
(** Binding for declaring the figure and intersection resulting from an intersection test handling the cases of void and source light intersection *)