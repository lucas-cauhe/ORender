(* Purely functional, functorized kd-tree algorithm from Overmars (2008).

   Credits to https://github.com/bpr/kd_tree
   Revisited by: Lucas Cauhé Viñao, 844665
   Andrei Dumbrava Luca, 844417
   Slightly modified version from source to implement KNN
*)
module type Multidim = Multidim.S

module type S = sig
  type real
  type point
  type elt

  (* Make t private and not abstract for debugging and fiddling at the top level *)
  type t = private
    | Leaf of elt
    | Branch of int * point * t * t

  val split : int -> elt list -> elt * elt list * elt list
  val make : elt list -> t

  (* kd tree functionality *)

  val nearest_neighbors : t -> point -> float -> elt list * float
end

(* A static KD tree *)

module Make (M : Multidim.S) :
  S with type elt = M.t with type real = M.real with type point = M.point = struct
  type elt = M.t
  type real = M.real
  type point = M.point

  type t =
    | Leaf of elt
    | Branch of int * point * t * t

  let list_split_at l n = BatList.take n l, BatList.drop n l

  let split (depth : int) (elts : elt list) : elt * elt list * elt list =
    let axis = depth mod M.dim in
    let cmpf e0 e1 = M.axial_compare axis (M.to_point e0) (M.to_point e1) in
    let len = List.length elts in
    let sorted = List.sort cmpf elts in
    let lt, gte = list_split_at sorted (len / 2) in
    List.hd gte, lt, gte
  ;;

  let make elts =
    let rec mk depth es =
      match es with
      | [ elt ] -> Leaf elt
      | _ ->
        let depth' = depth + 1 in
        let median, lt, gte = split depth es in
        Branch (depth mod M.dim, M.to_point median, mk depth' lt, mk depth' gte)
    in
    mk 0 elts
  ;;

  let nearest_neighbors root q radius =
    let squared_radius = Common.square radius in
    let update elt ((curr_closest, max_distance) as curr) =
      let p = M.to_point elt in
      let d_2 = M.squared_distance q p in
      if d_2 <= squared_radius then
        elt :: curr_closest, max max_distance d_2
      else
        curr
    in
    let rec descend node path curr =
      match node, path with
      | Leaf elt, _ -> ascend path (update elt curr)
      | Branch (axis, median, lt, gte), _ ->
        let n =
          if M.axial_compare axis q median >= 0 then
            gte
          else
            lt
        in
        descend n (node :: path) curr
    and ascend path curr =
      match path with
      | [] -> curr
      | node :: nodes -> ascend nodes (check_other_side node [] curr)
    and check_other_side node path curr =
      match node with
      | Branch (axis, median, lt, gte)
        when M.squared_axial_distance axis q median <= squared_radius ->
        if M.axial_compare axis q median >= 0 then
          descend lt path curr
        else
          descend gte path curr
      | _ -> curr
    in
    descend root [] ([], 0.)
  ;;
end
