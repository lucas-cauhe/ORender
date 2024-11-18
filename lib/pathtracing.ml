open Geometry
open Scene
open Colorspace
open Brdf

(** Traces a ray [ray] accross the given [scene] to find which is the first figure that intersects with the ray, if any *)
let trace_ray scene ray : Figures.scene_figure * Figures.intersection_result =
  match Figures.find_closest_figure scene ray with
  | Some (fig, ir) ->
    (match Figures.is_sphere fig, ir with
     | true, Intersects (intersection :: _) ->
       let surface_normal_ray =
         Figures.ray intersection.intersection_point intersection.surface_normal
       in
       let moved_ip = Figures.point_of_ray surface_normal_ray 10e-5 in
       fig, Intersects [ { intersection with intersection_point = moved_ip } ]
     | _ -> fig, ir)
  | None -> Figures.Figure (Figures.empty ()), Zero
;;

(** Cosine norm given a figure's intersection point surface normal and the outgoing direction wi. *)
let cosine_norm (n : Direction.direction_t) (wi : Direction.direction_t) =
  Direction.dot n wi |> abs_float
;;

(** Compute the direct light given a [Light.light_source] and a [Figures.intersection].
    Returns a function that expects the brdf value to finally compute value of the direct light *)
let direct_light
  (scene : Figures.scene)
  (ls : Light.light_source list)
  (x : Figures.intersection)
  : Rgb.pixel -> Rgb.pixel
  =
  let point_ligth_shadow_ray acc light =
    match Light.light_source_type_val light with
    | Point ls_center ->
      let dist_to_light =
        Direction.between_points x.intersection_point ls_center
        |> Direction.normalize
        |> Option.get
      in
      Rgb.value_prod
        (Light.shadow_ray scene x light)
        (cosine_norm x.surface_normal dist_to_light)
      |> Rgb.sum acc
    | Area _ -> Rgb.zero ()
  in
  Rgb.rgb_prod (List.fold_left point_ligth_shadow_ray (Rgb.zero ()) ls)
;;

let rec rec_path_tracing scene light_sources wi current_media =
  match trace_ray scene wi with
  | _, Zero -> Rgb.zero ()
  | _, Intersects (ir :: _)
    when Light.point_belongs_to_ls ir.intersection_point (List.hd light_sources) ->
    Light.power (List.hd light_sources)
  | fig, Intersects (ir :: _) ->
    (match russian_roulette (Figures.get_figure fig) with
     | Absorption, _ -> Rgb.zero ()
     | roulette_result, roulette_prob ->
       (* compute wi *)
       let outgoing_direction, next_media =
         montecarlo_sample
           (Figures.get_figure fig)
           ir
           wi.ray_direction
           current_media
           roulette_result
       in
       (* compute current brdf *)
       let current_brdf =
         brdf
           (Figures.get_figure fig)
           ir.surface_normal
           wi.ray_direction
           outgoing_direction
           (roulette_result, roulette_prob)
           current_media
       in
       let direct_light_contribution =
         if roulette_result = Diffuse then
           direct_light scene (Light.sample_light light_sources) ir current_brdf
         else
           Rgb.zero ()
       in
       let global_light_contribution =
         cosine_norm ir.surface_normal outgoing_direction |> Rgb.value_prod current_brdf
       in
       Rgb.rgb_prod
         global_light_contribution
         (rec_path_tracing
            scene
            light_sources
            (Figures.ray ir.intersection_point outgoing_direction)
            next_media)
       |> Rgb.sum direct_light_contribution
       |> Rgb.rgb_prod (Figures.get_figure fig |> Figures.emission))
    (* Rgb.rgb_prod direct_light_contribution (Figures.get_figure fig |> Figures.emission) *)
    (* Rgb.rgb_of_values
       (if Direction.z outgoing_direction >= 0. then
       1.
       else
       0.)
       (if Direction.z outgoing_direction < 0. then
       1.
       else
       0.)
       0.) *)
  | _ -> Rgb.zero ()
;;

(** Path tracing algorithm implementation *)
let path_tracing scene light_sources camera_ray =
  let ls_hd = List.hd light_sources in
  match Light.light_source_type_val ls_hd with
  | Light.Area fig -> rec_path_tracing (fig :: scene) light_sources camera_ray 1.
  | Light.Point _ -> rec_path_tracing scene light_sources camera_ray 1.
;;
