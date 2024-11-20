open Scene
open Colorspace
open Brdf

let ( let* ) rres f =
  match rres with
  | Absorption, _ -> Rgb.zero ()
  | roulette_result, roulette_prob -> f (roulette_result, roulette_prob)
;;

let ( let& ) (tracing_result, light_source) f =
  match tracing_result with
  | _, Figures.Zero -> Rgb.zero ()
  | _, Figures.Intersects (ir :: _)
    when Light.point_belongs_to_ls ir.intersection_point light_source ->
    Light.power light_source
  | fig, Figures.Intersects (ir :: _) -> f (fig, ir)
  | _ -> Rgb.zero ()
;;
