open Domainslib

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
  val show : config -> unit

  
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
  let show conf = Printf.printf "Normal: %s, Origin: %s" (Geometry.Direction.string_of_direction conf.normal) (Geometry.Point.string_of_point conf.origin)

  
  

end


module Sphere = struct
  type config = { center : Geometry.Point.t; radius : float }
   
  let init center radius = { center; radius }

  let center sphere = sphere.center

  let radius sphere = sphere.radius
  let show conf = Printf.printf "Center: %s, Radius: %f" (Geometry.Point.string_of_point conf.center) conf.radius

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
type scene = figure list

let closest_figure pool (scene : figure BatArray.t ) (intersections : float list BatArray.t) (ray : ray_type) = 
  Task.parallel_for pool ~start:0 ~finish:(BatArray.length scene - 1) ~body:(fun i -> match scene.(i) with
    | Plane(config) -> intersections.(i) <- Plane.intersects config ray
    | Sphere(config) -> intersections.(i) <- Sphere.intersects config ray
  );
  let min (fig_min, dist_min) ind next = match next with
  | [] -> (fig_min, dist_min)
  | curr :: _ -> if dist_min < curr then (fig_min, dist_min) else (scene.(ind), curr) in
  let fst_dist = List.hd intersections.(0) in
  let fst_fig = scene.(0) in
  let (result, _) = BatArray.fold_lefti min (fst_fig, fst_dist) intersections in
  result

let find_closest_figure s ray = 
  let pool = Task.setup_pool ~num_domains:7 () in
  let scene_arr = BatArray.of_list s in
  let intersections = BatArray.init (List.length s) (fun _ -> []) in
  let fig = Task.run pool (fun () -> closest_figure pool scene_arr intersections ray) in
  Task.teardown_pool pool;
  fig
  

let show_figure = function Plane(conf) -> Plane.show conf
  | Sphere(conf) -> Sphere.show conf

