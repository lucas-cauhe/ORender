open Geometry
open Colorspace
open Figures

type light_source = {
  ls_center: Point.t;
  ls_power: Rgb.pixel
}

let light_source c p = {
  ls_power = p;
  ls_center = c;
}

let shadow_ray scene intersection ls = 
  match  Direction.between_points ls.ls_center intersection.intersection_point |> Direction.normalize with
  | None -> ls.ls_power
  | Some(center_to_point) -> begin 
    let ray_to_ls = ray intersection.intersection_point center_to_point in
    match Figures.find_closest_figure scene ray_to_ls with
    | Some(_, Intersects({distance = dist; _} :: _)) 
      when dist < dist_to_point_of_ray ray_to_ls ls.ls_center -> Rgb.zero () 
    | _ ->
      Rgb.normalize ls.ls_power (Direction.modulus center_to_point |> Common.square)
  end