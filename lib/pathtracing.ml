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
    Geometry.cartesian_of_spherical rand_lat rand_azimut (Direction.normalize normal |> Option.get) ip |> Direction.normalize |> Option.get
  | Figures.Specular -> 
    Direction.dot wo normal |> ( *. ) 2. |> Direction.prod normal |> Direction.sub wo 
  | Figures.Refraction ->
    (* not yet implemented *)
    normal
  | Figures.Absorption -> normal

(**
  Render equation implementation
*)
(* let direct_light (light : Rgb.pixel) (brdf : float) (emission : Rgb.pixel) = 
  Rgb.value_prod light brdf |> Rgb.rgb_prod emission *)

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
    | Figures.Absorption -> acc 
    | roulette_result -> 
      (* compute wi  *)
      let outgoing_direction = montecarlo_sample ir.surface_normal wi.ray_direction ir.intersection_point roulette_result in
      (* compute current brdf *)
      let current_brdf = cosine_norm ir.surface_normal outgoing_direction |> Rgb.value_prod (Figures.brdf (Figures.get_figure fig) ir.surface_normal outgoing_direction roulette_result )  in
      let current_contribution = Rgb.rgb_prod acc current_brdf |> Rgb.rgb_prod (Figures.get_figure fig |> Figures.emission) in 
      area_light_path_tracing scene light_source
        (Figures.ray ir.intersection_point outgoing_direction)
        current_contribution 
  end
  | _ -> acc 


let point_light_path_tracing scene light_sources wi = 
  match trace_ray scene wi with
  | (_, Zero) -> Rgb.zero () 
  | (fig, Intersects(ir :: _)) -> begin
    match Figures.russian_roulette (Figures.get_figure fig) with
    | Figures.Absorption -> Rgb.zero ()
    | roulette_result ->  
      (* compute wi  *)
      let outgoing_direction = montecarlo_sample ir.surface_normal wi.ray_direction ir.intersection_point roulette_result in
      (* compute current brdf *)
      let current_brdf = Figures.brdf (Figures.get_figure fig) ir.surface_normal outgoing_direction roulette_result in
      let direct_light_cosine = Direction.dot ir.surface_normal ()
      let rec_brdf = current_brdf * wi_cos in
      let direct_brdf = current_brdf * direct_light_cosine in
      (* add all shadow rays' effect *) 
      List.fold_left (fun acc_lp ls -> Rgb.sum (Light.shadow_ray scene ir ls) direct_brdf |> Rgb.rgb_prod (Figures.get_figure fig |> Figures.emission) |> Rgb.sum acc_lp) (Rgb.zero ()) light_sources
      (* Rgb.rgb_prod (Figures.get_figure fig |> Figures.emission) (point_light_path_tracing scene light_sources (Figures.ray ir.intersection_point outgoing_direction)) |> Rgb.sum current_contribution *)
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
