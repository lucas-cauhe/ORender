type russian_roulette_result =
  | Absorption
  | Diffuse
  | Specular
  | Refraction

val russian_roulette : Scene.Figures.figure -> russian_roulette_result * float

val montecarlo_sample : 
    Geometry.Direction.direction_t -> 
    Geometry.Direction.direction_t -> 
    Geometry.Point.point_t ->
    russian_roulette_result ->
    Geometry.Direction.direction_t

val brdf : 
    Scene.Figures.figure -> 
    Geometry.Direction.direction_t -> 
    Geometry.Direction.direction_t ->
    Geometry.Direction.direction_t -> 
    russian_roulette_result * float -> 
    Colorspace.Rgb.pixel 