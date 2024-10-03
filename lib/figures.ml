type ray_type = {
  origin: Geometry.Point.t;
  direction: Geometry.Direction.t
}

module type Figure = sig
  type config

  (** Dada una figura y un rayo, devuelve una lista con las distancias a los puntos con los que intersecta
      el rayo en la figura
  *)
  val intersects : config -> ray_type -> float list

  
end

module Plane : Figure = struct
  type config = {
    normal: Geometry.Direction.t;
    origin: Geometry.Point.t;
  }

  (* Plane implicit equation: ax + by + cz + d = 0 *)
  (* Ray implicit equation: p + d * t = 0 *)
  let intersects plane (ray : ray_type) = 
    let c = Geometry.Direction.dot (Geometry.Direction.of_point plane.origin) plane.normal in
    let num = Geometry.Direction.dot (Geometry.Direction.of_point ray.origin) plane.normal |> ( +. ) (-.c) in
    let den = Geometry.Direction.dot ray.direction plane.normal in
    [-.num /. den]

  let init d o = { normal = d; origin = o }

  
  

end


module Sphere = struct
  type config = { center : Geometry.Point.t; radius : float }
   
  let create = { center; radius }

  let center sphere = sphere.center

  let radius sphere = sphere.radius

  let contains sphere point = 
    let distance = Geometry.Point.distance sphere.center point in
    distance <= sphere.radius

    let intersects sphere ray =
    let open Geometry.Point in
    let open Geometry.Direction in
    let oc = ray.origin - sphere.center in
    let a = dot ray.direction ray.direction in
    let b = 2.0 *. (dot oc ray.direction) in
    let c = (dot oc oc) -. (sphere.radius *. sphere.radius) in
    let discriminant = (b *. b) -. (4.0 *. a *. c) in
    if discriminant < 0.0 then
      []
    else
      let t1 = (-.b -. sqrt discriminant) /. (2.0 *. a) in
      let t2 = (-.b +. sqrt discriminant) /. (2.0 *. a) in
      if t1 > t2 then
        [t2; t1]
      else
        [t1; t2]
end