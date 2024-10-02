module Ray = struct
  type t = {
    origin: Geometry.Point.t;
    direction: Geometry.Direction.t
    }
end


module Plane = struct
  type config = {
    normal: Geometry.Direction.t;
    origin: Geometry.Point.t;
  }

  let init d o = { normal = d; origin = o }

  (* Plane implicit equation: ax + by + cz + d = 0 *)
  (* Ray implicit equation: p + d * t = 0 *)
  let intersects plane ray =
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