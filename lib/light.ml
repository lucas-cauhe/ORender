open Geometry
open Colorspace

type light_source_type = {
  ls_center: Point.t;
  ls_power: Rgb.pixel
}

let light_source c p = {
  ls_power = p;
  ls_center = c;
}

let shadow_ray scene (ir : Figures.intersection) ls = 
  match  Direction.between_points ls.ls_center ir.intersection_point |> Direction.normalize with
  | None -> ls.ls_power
  | Some(center_to_point) -> begin 
    let ray_to_ls = Figures.ray ir.intersection_point center_to_point in
    match Figures.find_closest_figure scene ray_to_ls with
    | Some(_, Intersects({distance = dist; _} :: _)) 
      when dist < Figures.dist_to_point_of_ray ray_to_ls ls.ls_center -> Rgb.rgb_of_values 0. 0. 0.
    | _ ->
      Rgb.normalize ls.ls_power (Direction.modulus center_to_point |> Common.square)
  end