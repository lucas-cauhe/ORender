open Scene.Figures
open Colorspace
open Geometry

type russian_roulette_result =
  | Absorption
  | Diffuse
  | Specular
  | Refraction

let absorption_prob = ref 0.2

let delta wr wi =
  if Direction.cross_product wr wi |> Direction.modulus < 0.001 then
    1.
  else
    0.
;;

let sample_specular wo normal =
  Direction.dot wo normal
  |> ( *. ) 2.
  |> Direction.prod normal
  |> Direction.sub wo
  |> Direction.normalize
  |> Option.get
;;

(* let sample_refraction w0 normal media_in media_out =
  let _theta_zero = Direction.angle normal w0 in
  let cosTh = min (Direction.dot (Direction.inv w0) normal) 1. in
  let sinTh = sqrt (1. -. Common.square cosTh) in
  let entrance = Direction.dot w0 normal < 0. in
  let inv_normal =
    if entrance then
      normal
    else
      Direction.inv normal
  in
  let max_refraction =
    if entrance then
      1. /. media_in 
    else
      media_out
  in
  if max_refraction *. sinTh > 1. then
    sample_specular w0 inv_normal, media_in
  else (
    let cos_out = min (Direction.dot (Direction.inv w0) inv_normal) 1. in
    let r_perp =
      Direction.prod
        (Direction.prod inv_normal cos_out |> Direction.sum w0)
        max_refraction
    in
    let r_parallel =
      sqrt @@ abs_float @@ (1. -. Direction.dot r_perp r_perp)
      |> Direction.prod (Direction.inv inv_normal)
    in
    ( Direction.sum r_perp r_parallel
    , if entrance then
        media_out
      else
        media_in ) *)
(* let thetai = Float.asin @@ (media_in /. media_out *. Float.sin theta_zero) in
    ( Direction.prod inv_normal (Float.cos thetai) |> Direction.normalize |> Option.get
    , if media_in = media_out then
        1.
      else
        media_out ) *)

let sample_refraction w0 normal media_in media_out =
  let theta_zero = Direction.angle normal w0 in
  let entrance = Direction.dot w0 normal < 0. in
  let media_out =
    if entrance then
      media_out
    else
      1.
  in
  let inv_normal =
    if entrance then
      normal
    else
      Direction.inv normal
  in
  let theta_critical = Float.asin @@ (media_out /. media_in) in
  if theta_zero >= theta_critical then
    sample_specular w0 inv_normal, media_out
  else (
    let incident = Direction.prod w0 (media_in /. media_out) in
    let thetai = Float.asin @@ (media_in /. media_out *. Float.sin theta_zero) in
    let out =
      Direction.prod inv_normal ((media_in /. media_out *. cos theta_zero) -. cos thetai)
    in
    Direction.sum incident out, media_out
  )
;;

let montecarlo_sample
  fig
  { surface_normal = normal; intersection_point = ip; _ }
  wo
  scene_media
  = function
  | Diffuse ->
    let rand_lat = 1. -. Random.float 1. |> sqrt |> Float.acos in
    let rand_azimut = 2. *. Float.pi *. Random.float 1. in
    (* It is impossible that the modulus of the global coords' direction is ever 0, hence can do Option.get *)
    let normal_normalized = Direction.normalize normal |> Option.get in
    let wi_prime =
      Geometry.cartesian_of_spherical rand_lat rand_azimut normal_normalized
      |> Geometry.Transformations.hc_of_direction
    in
    let cb_mat = Geometry.cb_matrix_tangent normal_normalized ip in
    ( Geometry.Transformations.change_basis cb_mat wi_prime
      |> Geometry.Transformations.direction_of_hc
    , scene_media )
  | Specular -> sample_specular wo normal, scene_media
  | Refraction ->
    let media_out = refraction fig in
    sample_refraction wo normal scene_media media_out
  | Absorption -> normal, scene_media
;;

let russian_roulette (fig : figure) =
  let coefs = ref [] in
  let kd, ks, kt = coefficients fig in
  let rgb_internal_sum rgb = Rgb.red rgb +. Rgb.green rgb +. Rgb.blue rgb in
  if rgb_internal_sum kd > 0. then coefs := Diffuse :: !coefs;
  if rgb_internal_sum ks > 0. then coefs := Specular :: !coefs;
  if rgb_internal_sum kt > 0. then coefs := Refraction :: !coefs;
  if Random.float 1. < !absorption_prob then
    Absorption, !absorption_prob
  else
    ( List.nth !coefs (Random.int (List.length !coefs))
    , (1. -. !absorption_prob) /. (List.length !coefs |> float_of_int) )
;;

let brdf fig n w0 wi (rres, prob) scene_media =
  let kd, ks, kt = coefficients fig in
  match rres with
  | Absorption -> Rgb.zero ()
  | Diffuse -> Rgb.normalize kd prob (* uniform cosine sampling *)
  | Specular ->
    let wr = sample_specular w0 n in
    Rgb.normalize (Rgb.value_prod ks (delta wr wi)) (Direction.dot n wi)
  | Refraction ->
    let media_out = refraction fig in
    let wr, _ = sample_refraction w0 n scene_media media_out in
    Rgb.normalize (Rgb.value_prod kt (delta wr wi)) (Direction.dot n wi)
;;
