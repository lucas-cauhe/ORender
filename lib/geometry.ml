(** Geometry module contains basic mathematical utilities for the project *)

(** Associate functions to work with points *)
module type PointType = sig
  (** Point internal type *)
  type t

  (** Sum of two points *)
  val sum : t -> t -> t

  (** Subtraction of two points *)
  val sub : t -> t -> t

  (** Product between point and scalar value *)
  val prod : t -> float -> t
  
  (** Division between point and scalar value.
   Returns [None] if [num] is 0 *)
  val div : t -> float -> t option
  val from_coords : float -> float -> float -> t
  val string_of_point : t -> string
  val eq : t -> t -> bool
end 

(** Implementation of the [PoinType] functions *)
module Point : PointType = struct
  type t = {
    x: float;
    y: float;
    z: float;
  }
  let sum p1 p2 = {x = p1.x +. p2.x; y = p1.y +. p2.y; z = p1.z +. p2.z}

  let sub p1 p2 =  {x = p1.x -. p2.x; y = p1.y -. p2.y; z = p1.z -. p2.z}

  let prod p num = {x = p.x *. num; y = p.y *. num; z = p.z *. num}

  let div p = function 0. -> None 
  | num -> Some({x = p.x /. num; y = p.y /. num; z = p.z /. num})

  let from_coords x y z = { x; y; z }
  let string_of_point p = Printf.sprintf "Point {x = %f; y = %f; z = %f}" p.x p.y p.z
  let eq p1 p2 = p1.x = p2.x && p1.y = p2.y && p1.z = p2.z
end

module type DirectionType = sig
  type t
  val sum : t -> t -> t
  val sub : t -> t -> t
  val prod : t -> float -> t
  val div : t -> float -> t option
  val dot : t -> t -> float
  val modulus : t -> float
  val normalize : t -> t option
  val cross_product : t -> t -> t
  val from_coords : float -> float -> float -> t
  val string_of_direction : t -> string
  val eq : t -> t -> bool
end 

module Direction : DirectionType = struct
  type t = {
    x: float;
    y: float;
    z: float;
  }
  let sum d1 d2 = {x = d1.x +. d2.x; y = d1.y +. d2.y; z = d1.z +. d2.z}

  let sub d1 d2 =  {x = d1.x -. d2.x; y = d1.y -. d2.y; z = d1.z -. d2.z}

  let prod d num = {x = d.x *. num; y = d.y *. num; z = d.z *. num}

  let div d = function 0. -> None 
  | num -> Some({x = d.x /. num; y = d.y /. num; z = d.z /. num})

  let dot d1 d2 = d1.x *. d2.x +. d1.y *. d2.y +. d1.z *. d2.z
  let modulus d = sqrt (d.x *. d.x +. d.y *. d.y +. d.z *. d.z)

  let normalize d = modulus d |> div d

  let cross_product d1 d2 = { x = d1.y *. d2.z -. d1.z *. d2.y; y = d1.z *. d2.x -. d1.x *. d2.z; z = d1.x *. d2.y -. d1.y *. d2.x }

  let from_coords x y z = { x; y; z }
  let string_of_direction d = Printf.sprintf "Direction {x = %f; y = %f; z = %f}" d.x d.y d.z
  let eq d1 d2 = d1.x = d2.x && d1.y = d2.y && d1.z = d2.z
end

