type russian_roulette_result =
  | Absorption
  | Diffuse
  | Specular
  | Refraction

val russian_roulette : Scene.Figures.figure -> russian_roulette_result * float

val montecarlo_sample : 
  Scene.Figures.figure ->
    Scene.Figures.intersection ->
    Geometry.Direction.direction_t -> 
      float ->
    russian_roulette_result ->
    Geometry.Direction.direction_t * float

val brdf : 
    Scene.Figures.figure -> 
    Geometry.Direction.direction_t -> 
    Geometry.Direction.direction_t ->
    Geometry.Direction.direction_t -> 
    russian_roulette_result * float -> 
      float ->
    Colorspace.Rgb.pixel 