type ray_type = {
  origin: Geometry.Point.t;
  direction: Geometry.Direction.t
}

let square (n : float) : float = n *. n

module type FigureSig = sig
  type config

  (** Dada una figura y un rayo, devuelve una lista con las distancias a los puntos con los que intersecta
      el rayo en la figura
  *)
  val intersects : config -> ray_type -> float list

  
end


module Plane = struct
  type config = {
    normal: Geometry.Direction.t;
    origin: Geometry.Point.t;
  }

  (* Plane implicit equation: ax + by + cz + d = 0 *)
  (* Ray implicit equation: p + d * t = 0 *)
  let intersects plane (ray : ray_type) = 
    let open Geometry.Direction in
    (* Check if ray and plane are parallel *)
    match plane.normal * ray.direction with
    | 0. -> []
    | den ->
      let c = (of_point plane.origin) * plane.normal in
      let num = (of_point ray.origin) * plane.normal |> ( +. ) (-.c) in
      [-.num /. den]

  let init d o = { normal = d; origin = o }

  
  

end


module Sphere = struct
  type config = { center : Geometry.Point.t; radius : float }
   
  let init center radius = { center; radius }

  let center sphere = sphere.center

  let radius sphere = sphere.radius

  let intersects sphere ray =
  let module GPoint = Geometry.Point in
  let module GDirection = Geometry.Direction in
  let oc = GPoint.sub ray.origin sphere.center in
  let a = GDirection.modulus ray.direction |> square in
  let b = 2.0 *. (GDirection.dot (GDirection.of_point oc) ray.direction) in
  let c = (GPoint.distance ray.origin sphere.center |> square) -. (square sphere.radius) in
  let discriminant = (b *. b) -. (4.0 *. a *. c) in
  if discriminant < 0.0 then
    []
  else
    let t1 = (-.b -. sqrt discriminant) /. (2.0 *. a) in
    let t2 = (-.b +. sqrt discriminant) /. (2.0 *. a) in
    if t1 > t2 then [t2; t1] else [t1; t2]
end

type figure = Plane of Plane.config | Sphere of Sphere.config
