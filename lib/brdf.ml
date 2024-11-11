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

let sample_refraction w0 normal media_in media_out =
  let theta_zero = Direction.angle normal w0 in
  let thetai = Float.asin @@ (media_out /. media_in *. Float.sin theta_zero) in
  let inv_normal = Direction.prod normal (-1.) in
  Direction.prod inv_normal (Float.cos thetai) |> Direction.normalize |> Option.get
;;

let montecarlo_sample normal wo ip = function
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
    Geometry.Transformations.change_basis cb_mat wi_prime
    |> Geometry.Transformations.direction_of_hc
  | Specular -> sample_specular wo normal
  | Refraction -> normal
  | Absorption -> normal
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

let brdf fig n w0 wi (rres, prob) =
  let kd, ks, kt = coefficients fig in
  match rres with
  | Absorption -> Rgb.zero ()
  | Diffuse -> Rgb.normalize kd prob (* uniform cosine sampling *)
  | Specular ->
    let wr = sample_specular w0 n in
    Rgb.normalize (Rgb.value_prod ks (delta wr wi)) (Direction.dot n wi)
  | Refraction ->
    let wr = sample_refraction w0 n 1. 1. in
    Rgb.normalize (Rgb.value_prod kt (delta wr wi)) (Direction.dot n wi)
;;
