
type matrix_t = float array array

let identity dim = Array.init_matrix dim dim (fun i j -> if i = j then 1. else 0.)

let get_element m i j = m.(i).(j)

let transpose m = 
  let dimy = Array.length m in
  let dimx = Array.length m.(0) in
  Array.init_matrix dimx dimy (fun i j -> m.(j).(i))

let from_array_matrix mat = 
  let dimy = Array.length mat in
  let dimx = Array.length mat.(0) in
  Array.init_matrix dimy dimx (fun i j -> mat.(i).(j))

let string_of_row = Array.fold_left (fun acc el -> acc ^ (Printf.sprintf "%f " el ) ) ""

let string_of_matrix = 
  Array.fold_left (fun result nextRow -> result ^ "| " ^ (string_of_row nextRow) ^ "|\n") ""

let multiply m1 m2 =
  let x0 = Array.length m1 and y0 = Array.length m2 and
    x1 = Array.length m1.(0) and y1 = Array.length m2.(0) in
  if x1 <> y0 then
    None
  else
    let m' = Array.make_matrix (Array.length m1) (Array.length m2.(0)) 0.0 in
    for i = 0 to x0 - 1 do
      for j = 0 to y1 - 1 do
        for k = 0 to x1 - 1 do
          m'.(i).(j) <- m'.(i).(j) +. m1.(i).(k) *. m2.(k).(j)
        done
      done
    done;
    Some(m')

  (* Function to get the submatrix by removing a specific row and column *)
  let submatrix mat row col =
    let n = Array.length mat in
    Array.init (n - 1) (fun i ->
      Array.init (n - 1) (fun j ->
        mat.(if i < row then i else i + 1).(if j < col then j else j + 1)
      )
    )

  (* Recursive function to calculate the determinant of an NxN matrix *)
  let rec determinant mat =
    let n = Array.length mat in
    if n = 1 then mat.(0).(0)
    else if n = 2 then mat.(0).(0) *. mat.(1).(1) -. mat.(0).(1) *. mat.(1).(0)
    else
      let rec aux acc i =
        if i >= n then acc
        else
          let sign = if i mod 2 = 0 then 1.0 else -1.0 in
          let subm = submatrix mat 0 i in
          aux (acc +. sign *. mat.(0).(i) *. determinant subm) (i + 1)
      in
      aux 0.0 0

  (* Function to calculate the cofactor matrix *)
  let cofactor_matrix mat =
    let n = Array.length mat in
    Array.init n (fun i ->
      Array.init n (fun j ->
        let sign = if (i + j) mod 2 = 0 then 1.0 else -1.0 in
        sign *. determinant (submatrix mat i j)
      )
    )

  (* Function to calculate the inverse of a matrix *)
  let inverse mat =
    let det = determinant mat in
    if det = 0.0 then
      let _ = print_endline "Matrix has det 0" in
      None
    else
      let cofactor_mat = cofactor_matrix mat in
      let adjugate = transpose cofactor_mat in
      let n = Array.length mat in
      Some (Array.init n (fun i ->
        Array.init n (fun j ->
          adjugate.(i).(j) /. det
        )
      ))