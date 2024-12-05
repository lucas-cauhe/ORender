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
  type range
  type elt

  (* Make t private and not abstract for debugging and fiddling at the top level *)
  type t = private
    | Leaf of elt
    | Branch of int * point * range * t * t

  val split : int -> elt list -> elt * range * elt list * elt list
  val make : elt list -> t

  (* kd tree functionality *)

  val nearest_neighbors : t -> point -> float -> elt list * float
  val range_search : t -> range -> elt list
end

(* A static KD tree *)

module Make (M : Multidim.S) :
  S
  with type elt = M.t
  with type real = M.real
  with type point = M.point
  with type range = M.range = struct
  type elt = M.t
  type real = M.real
  type range = M.range
  type point = M.point

  type t =
    | Leaf of elt
    | Branch of int * point * range * t * t

  let list_split_at l n = BatList.take n l, BatList.drop n l

  let split (depth : int) (elts : elt list) : elt * range * elt list * elt list =
    let axis = depth mod M.dim in
    let cmpf e0 e1 = M.axial_compare axis (M.to_point e0) (M.to_point e1) in
    let len = List.length elts in
    let range = List.fold_left M.range_maker M.null_range elts in
    let sorted = List.sort cmpf elts in
    let lt, gte = list_split_at sorted (len / 2) in
    List.hd gte, range, lt, gte
  ;;

  let make elts =
    let rec mk depth es =
      match es with
      | [ elt ] -> Leaf elt
      | _ ->
        let depth' = depth + 1 in
        let median, range, lt, gte = split depth es in
        Branch (depth mod M.dim, M.to_point median, range, mk depth' lt, mk depth' gte)
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
      (* if d_2 < curr_distance then
         [ elt ], d_2
         else if d_2 > curr_distance then
         curr
         else
         (* d_2 = curr_distance, so merge *)
         elt :: curr_closest, d_2 *)
    in
    let rec descend node path curr =
      match node, path with
      | Leaf elt, _ -> ascend path (update elt curr)
      | Branch (axis, median, _, lt, gte), _ ->
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
      | Branch (axis, median, _, lt, gte)
        when M.squared_axial_distance axis q median <= squared_radius ->
        if M.axial_compare axis q median >= 0 then
          descend lt path curr
        else
          descend gte path curr
      | _ -> curr
    in
    descend root [] ([], 0.)
  ;;

  let range_search t r =
    let rec search curr accum =
      match curr with
      | Leaf elt ->
        if M.point_in_range r (M.to_point elt) then
          elt :: accum
        else
          accum
      | Branch (_, _, range, lt, gte) ->
        let range_intersection = M.intersect_ranges r range in
        if range_intersection <> M.null_range then (
          let accum' = search lt accum in
          search gte accum'
        ) else
          accum
    in
    search t []
  ;;
end
