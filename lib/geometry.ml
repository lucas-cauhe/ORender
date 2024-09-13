(** Geometry module contains basic mathematical utilities for the project *)
open Array

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

module type MatrixType = sig
  type m
  val identity : m
  val transpose : m -> m
  val translate : m -> Point.t -> m
  val scale : m -> Point.t -> m
  val rotate : m -> Direction.t -> float -> m
  val multiply : m -> m -> m
  val from_coords : float -> float -> float -> float -> float -> float -> float -> float -> float -> float -> float -> float -> float -> float -> float -> float -> m
  val string_of_matrix : m -> string
end


open Point 

(** Matrix struct implementation*)
(** TODO:
* Correct the point and direction refs
*)
module Matrix: MatrixType = struct
  type m = float array array

  let identity = [|
    [|1.0; 0.0; 0.0; 0.0|];
    [|0.0; 1.0; 0.0; 0.0|];
    [|0.0; 0.0; 1.0; 0.0|];
    [|0.0; 0.0; 0.0; 1.0|];
  |]

  let transpose m = 
    let n = Array.length m in
    let m' = Array.make_matrix n n 0.0 in
    for i = 0 to n - 1 do
      for j = 0 to n - 1 do
        m'.(i).(j) <- m.(j).(i)
      done
    done;
    m'
  (**Trying to fix this error
    File "lib/geometry.ml", line 126, characters 33-34:
    126 |     m'.(0).(3) <- m.(0).(3) +. p.x;
                                       ^
    Error: Unbound record field x
  *)
  let translate m (p: Point.t) =
    let m' = Array.copy m in
    m'.(0).(3) <- m.(0).(3) +. p.x;
    m'.(1).(3) <- m.(1).(3) +. p.y;
    m'.(2).(3) <- m.(2).(3) +. p.z;
    m'
  
  let scale m p =
    let m' = Array.copy m in
    m'.(0).(0) <- m.(0).(0) *. p.x;
    m'.(1).(1) <- m.(1).(1) *. p.y;
    m'.(2).(2) <- m.(2).(2) *. p.z;
    m'

  let rotate m d angle =
    let c = cos angle in
    let s = sin angle in
    let t = 1.0 -. c in
    let x = d.x in
    let y = d.y in
    let z = d.z in
    let m' = Array.copy m in
    m'.(0).(0) <- t *. x *. x +. c;
    m'.(0).(1) <- t *. x *. y -. s *. z;
    m'.(0).(2) <- t *. x *. z +. s *. y;
    m'.(1).(0) <- t *. x *. y +. s *. z;
    m'.(1).(1) <- t *. y *. y +. c;
    m'.(1).(2) <- t *. y *. z -. s *. x;
    m'.(2).(0) <- t *. x *. z -. s *. y;
    m'.(2).(1) <- t *. y *. z +. s *. x;
    m'.(2).(2) <- t *. z *. z +. c;
    m'

  let multiply m1 m2 =
    let m' = Array.make_matrix 4 4 0.0 in
    for i = 0 to 3 do
      for j = 0 to 3 do
        for k = 0 to 3 do
          m'.(i).(j) <- m'.(i).(j) +. m1.(i).(k) *. m2.(k).(j)
        done
      done
    done;
    m'


  let from_coords a11 a12 a13 a14 a21 a22 a23 a24 a31 a32 a33 a34 a41 a42 a43 a44 = [|
    [|a11; a12; a13; a14|];
    [|a21; a22; a23; a24|];
    [|a31; a32; a33; a34|];
    [|a41; a42; a43; a44|];
  |]

  let string_of_matrix m = 
    let s = ref "" in
    for i = 0 to 3 do
      for j = 0 to 3 do
        s := !s ^ (Printf.sprintf "%f " m.(i).(j))
      done;
      s := !s ^ "\n"
    done;
    !s



end