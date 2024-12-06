
(**
  Result of the russian roulette
*)
type russian_roulette_result =
  | Absorption
  | Diffuse
  | Specular
  | Refraction

(**
  Sample a spherical direction using uniform cosine angle

  [uniform_cosine_sample surface_normal intersection_point] samples a direction from the given [intersection_point]
*)
val uniform_cosine_sample : Geometry.Direction.direction_t -> Geometry.Point.point_t -> Geometry.Direction.direction_t

(**
  Sample a spherical direction using uniform solid angle

  [uniform_solid_angle_sample surface_normal intersection_point] samples a direction from the given [intersection_point]
*)
val uniform_solid_angle_sample : Geometry.Direction.direction_t -> Geometry.Point.point_t -> Geometry.Direction.direction_t

(**
  Run russian roulette for a given figure

  [russian_roulette fig] returns a tuple [(material, prob) as roulette_result] holding the material type and the probability of it being selected in the roulette
*)
val russian_roulette : Scene.Figures.figure -> russian_roulette_result * float

(**
  Sample a direction using montecarlo distribution

  [montecarlo_sample fig fig_intersection ray_direction material] returns an [outgoing_direction] with origin [fig_intersection.intersection_point] 
*)
val montecarlo_sample : 
  Scene.Figures.figure ->
    Scene.Figures.intersection ->
    Geometry.Direction.direction_t -> 
    russian_roulette_result ->
    Geometry.Direction.direction_t

(**
  Compute the bxdf function for a given figure

  [brdf fig surface_normal ray_direction outgoing_direction roulette_result] returns the colored pixel from applying the function on the intersection point
*)
val brdf : 
    Scene.Figures.figure -> 
    Geometry.Direction.direction_t -> 
    Geometry.Direction.direction_t ->
    Geometry.Direction.direction_t -> 
    russian_roulette_result * float -> 
    Colorspace.Rgb.pixel 

(**
  Check whether a figure follows a delta distribution

  [is_delta fig] returns a tuple [((material, prob), is_delta)]
*)
val is_delta : Scene.Figures.figure -> (russian_roulette_result*float) * bool

val cosine_norm : Geometry.Direction.direction_t -> Geometry.Direction.direction_t -> float
(** Cosine norm given a figure's intersection point surface normal and the outgoing direction wi. *)