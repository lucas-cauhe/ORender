open Colorspace
open Geometry
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


(**
  Render equation implementation
*)
let render_equation (light : Rgb.pixel) (brdf : float) (emission : Rgb.pixel) = 
  Rgb.value_prod light brdf |> Rgb.rgb_prod emission

(**
  Cosine norn given a figure's intersection point surface normal and the outgoing direction wi.
*)
let consine_norm (n : Direction.t) (wi : Direction.t) = 
  Direction.dot n wi |> abs_float
(**
  Path tracing algorithm implementation
*)
let path_tracing scene light_sources camera_ray = 
  let rec rec_path_tracing wi prev_brdf acc = 
    match trace_ray scene wi with
    | (_, Zero) -> acc
    | (fig, Intersects(ir :: _)) -> begin
      (* compute wi  *)
      let outgoing_direction = in
      (* compute current brdf *)
      let current_brdf = Figures.brdf fig ir.surface_normal outgoing_direction |> ( *. ) (cosine_norm ir.surface_normal outgoing_direction) in
      (* add all shadow rays' effect *)
      let light_power = List.fold_left (fun acc_lp ls -> Light.shadow_ray scene ir ls |> Rgb.sum acc_lp) (Rgb.zero ()) light_sources in
      let current_contribution = render_equation light_power current_brdf (Figures.get_figure fig |> Figures.emission) in
      rec_path_tracing 
        outgoing_direction
        (prev_brdf *. current_brdf)
        (Rgb.sum acc current_contribution) 
    end
    | _ -> acc in
      
  rec_path_tracing camera_ray 1. (Rgb.zero ()) 
