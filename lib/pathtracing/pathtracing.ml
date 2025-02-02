open Geometry
open Scene
open Scene.Figures
open Bindings
open Colorspace
open Brdf
module Bindings = Bindings

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

let direct_light scene ls x =
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

let rec rec_path_tracing scene light_sources texture_map wi =
  let& fig, ir = trace_ray scene wi, light_sources in
  let* roulette_result, roulette_prob = russian_roulette (Figures.get_figure fig) in
  (* compute wi *)
  let outgoing_direction =
    montecarlo_sample (Figures.get_figure fig) ir wi.ray_direction roulette_result
  in
  (* compute current brdf *)
  let current_brdf =
    brdf
      (Figures.get_figure fig)
      ir.surface_normal
      wi.ray_direction
      outgoing_direction
      (roulette_result, roulette_prob)
  in
  let direct_light_contribution =
    if roulette_result = Diffuse then
      direct_light scene light_sources ir current_brdf
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
       texture_map
       (Figures.ray ir.intersection_point outgoing_direction))
  |> Rgb.sum direct_light_contribution
  |> Rgb.rgb_prod
       (Figures.get_figure fig |> Figures.emission ir.intersection_point texture_map)
;;

(** Path tracing algorithm implementation *)
let path_tracing scene light_sources texture_map camera_ray =
  let lights_to_figs =
    List.fold_left
      (fun acc fig ->
        match Light.light_source_type_val fig with
        | Light.Area l -> l :: acc
        | _ -> acc)
      []
      light_sources
  in
  rec_path_tracing (lights_to_figs @ scene) light_sources texture_map camera_ray
;;
