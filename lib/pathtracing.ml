open Geometry
open Colorspace

(**
  Traces a ray [ray] accross the given [scene] to find which is the first figure that intersects with the ray, if any
*)
let trace_ray scene ray : Figures.scene_figure * Figures.intersection_result =
  match Figures.find_closest_figure scene ray with 
  | Some(fig, ir) -> begin
    match Figures.is_sphere fig, ir with
    | true, Intersects(intersection :: _) -> 
      let surface_normal_ray = Figures.ray intersection.intersection_point intersection.surface_normal in
      let moved_ip = Figures.point_of_ray surface_normal_ray 10e-5 in
      (fig, Intersects([{ intersection with intersection_point = moved_ip}]))
    | _ -> (fig, ir)
  end
  | None -> (Figures.Figure(Figures.empty ()), Zero)

let montecarlo_sample (normal : Direction.t) (wo : Direction.t) (ip : Point.t) = function 
  Figures.Diffuse ->
    let rand_lat = 1. -. Random.float 1. |> sqrt |> Float.acos in
    let rand_azimut = 2. *. Float.pi *. Random.float 1. in
    (* It is impossible that the modulus of the global coords' direction is ever 0, hence can do Option.get *)
    let normal_normalized = Direction.normalize normal |> Option.get in

    let wi_prime = Geometry.cartesian_of_spherical rand_lat rand_azimut normal_normalized 
      |> Geometry.Transformations.hc_of_direction in

    let cb_mat = Geometry.cb_matrix_tangent normal_normalized ip in 
    Geometry.Transformations.change_basis cb_mat wi_prime |> Geometry.Transformations.direction_of_hc
  | Figures.Specular -> 
    Direction.dot wo normal |> ( *. ) 2. |> Direction.prod normal |> Direction.sub wo 
  | Figures.Refraction ->
    (* not yet implemented *)
    normal
  | Figures.Absorption -> normal

(**
  Compute the direct light given a [Light.light_source] and a [Figures.intersection].
  Returns a function that expects the brdf value to finally compute value of the direct light 
*)
let direct_light (scene : Figures.scene) (light : Light.light_source) (x : Figures.intersection) : Rgb.pixel -> Rgb.pixel = 
  match Light.light_source_type_val light with
  | Point(ls_center) -> 
    let cosine_norm = Direction.between_points x.intersection_point ls_center 
      |> Direction.normalize 
      |> Option.get 
      |> Direction.dot x.surface_normal 
      |> abs_float in
    Rgb.rgb_prod (Rgb.value_prod (Light.shadow_ray scene x light) cosine_norm)
  | Area(_) -> Rgb.rgb_prod (Rgb.zero ())


(**
  Cosine norn given a figure's intersection point surface normal and the outgoing direction wi.
*)
let cosine_norm (n : Direction.t) (wi : Direction.t) = 
  Direction.dot n wi |> abs_float


let rec area_light_path_tracing scene light_source wi acc = 
  match trace_ray scene wi with
  | (_, Zero) -> acc
  | (_, Intersects(ir :: _)) when Light.point_belongs_to_ls ir.intersection_point light_source -> Rgb.rgb_prod (Light.power light_source) acc 
  | (fig, Intersects(ir :: _)) -> begin
    match Figures.russian_roulette (Figures.get_figure fig) with
    | Figures.Absorption, _  -> acc 
    | roulette_result, roulette_prob -> 
      (* compute wi  *)
      let outgoing_direction = montecarlo_sample ir.surface_normal wi.ray_direction ir.intersection_point roulette_result in
      (* compute current brdf *)
      let current_brdf = cosine_norm ir.surface_normal outgoing_direction |> Rgb.value_prod (Figures.brdf (Figures.get_figure fig) ir.surface_normal outgoing_direction (roulette_result, roulette_prob) )  in
      let current_contribution = Rgb.rgb_prod acc current_brdf |> Rgb.rgb_prod (Figures.get_figure fig |> Figures.emission) in 
      area_light_path_tracing scene light_source
        (Figures.ray ir.intersection_point outgoing_direction)
        current_contribution 
  end
  | _ -> acc 


let rec point_light_path_tracing scene light_sources wi = 
  match trace_ray scene wi with
  | (_, Zero) -> Rgb.zero () 
  | (fig, Intersects(ir :: _)) -> begin
    match Figures.russian_roulette (Figures.get_figure fig) with
    | Figures.Absorption, _ -> Rgb.zero ()
    | roulette_result, roulette_prob ->  
      (* compute wi  *)
      let outgoing_direction = montecarlo_sample ir.surface_normal wi.ray_direction ir.intersection_point roulette_result in
      (* compute current brdf *)
      let current_brdf = Figures.brdf (Figures.get_figure fig) ir.surface_normal outgoing_direction (roulette_result, roulette_prob) in
      let direct_light_contribution = direct_light scene (List.hd light_sources) ir current_brdf in 
      let global_light_contribution = cosine_norm ir.surface_normal outgoing_direction 
        |> Rgb.value_prod current_brdf
        |> Rgb.rgb_prod (Figures.get_figure fig |> Figures.emission) in
      Rgb.rgb_prod global_light_contribution (point_light_path_tracing scene light_sources (Figures.ray ir.intersection_point outgoing_direction)) |> Rgb.sum direct_light_contribution 
      (* Rgb.rgb_prod direct_light_contribution (Figures.get_figure fig |> Figures.emission) *)
      (* Rgb.rgb_of_values (Direction.x outgoing_direction) (Direction.y outgoing_direction) (Direction.z outgoing_direction) *)
  end
  | _ -> Rgb.zero () 


(**
  Path tracing algorithm implementation
*)
let path_tracing scene light_sources camera_ray = 
  let ls_hd = List.hd light_sources in
  match Light.light_source_type_val ls_hd with
  | Light.Area(fig) -> area_light_path_tracing (fig :: scene) ls_hd camera_ray (Rgb.rgb_of_values 1. 1. 1.) 
  | Light.Point(_) -> point_light_path_tracing scene light_sources camera_ray
