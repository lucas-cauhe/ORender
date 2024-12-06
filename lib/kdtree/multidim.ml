module type S = sig
  (* Dimension of the space we're interested in *)
  val dim : int

  (* The "real" or floating point number system for location and distance *)
  type real

  (* The n dimensional real values point *)
  type point

  (* The actual element (eg, a city) with a point *)
  type t

  val to_point : t -> point
  val axial_compare : int -> point -> point -> int
  val squared_distance : point -> point -> float
  val squared_axial_distance : int -> point -> point -> float
end
