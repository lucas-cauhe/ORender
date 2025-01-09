open Colorspace

type vertex =
  { x : float
  ; y : float
  ; z : float
  }

type normal =
  { nx : float
  ; ny : float
  ; nz : float
  }

type face =
  { v1 : int * int * int
  ; v2 : int * int * int
  ; v3 : int * int * int
  }

type texture =
  { u : float
  ; v : float
  }

let parse_vertex line = Scanf.sscanf line "v %f %f %f" (fun x y z -> { x; y; z })
let parse_normal line = Scanf.sscanf line "vn %f %f %f" (fun nx ny nz -> { nx; ny; nz })

let parse_face line =
  try
    Scanf.sscanf line "f %d/%d/%d %d/%d/%d %d/%d/%d" (fun p1 t1 n1 p2 t2 n2 p3 t3 n3 ->
      { v1 = p1, t1, n1; v2 = p2, t2, n2; v3 = p3, t3, n3 })
  with
  | _ ->
    Scanf.sscanf line "f %d/%d %d/%d %d/%d" (fun p1 t1 p2 t2 p3 t3 ->
      { v1 = p1, t1, 1; v2 = p2, t2, 1; v3 = p3, t3, 1 })
;;

let parse_texture line =
  Scanf.sscanf line "vt %f %f" (fun u v -> min 1. (max u 0.), 1. -. min 1. (max 0. v))
;;

let parse_mtl_file line = Scanf.sscanf line "mtllib %s" (fun s -> s)

let read_mtl_file filename =
  let kd = ref (Rgb.rgb_of_values 0.8 0.8 0.8) in
  let ks = ref (Rgb.rgb_of_values 0.0 0.0 0.0) in
  let ka = ref (Rgb.rgb_of_values 0.0 0.0 0.0) in
  let texture_file = ref "" in
  let ic = open_in ("mtl/" ^ filename) in
  let parse_color line prefix =
    if String.length line > 0 && line.[0] = '\t' then
      Scanf.sscanf
        line
        (Scanf.format_from_string ("\t" ^ prefix ^ " %f %f %f") "%f %f %f")
        (fun r g b -> Rgb.rgb_of_values r g b)
    else
      Scanf.sscanf
        line
        (Scanf.format_from_string (prefix ^ " %f %f %f") "%f %f %f")
        (fun r g b -> Rgb.rgb_of_values r g b)
  in
  let rec read_lines () =
    try
      let line = input_line ic in
      if String.length line > 0 && line.[0] <> '#' then (
        match String.trim line with
        | line when String.length line >= 2 ->
          (match String.sub (String.trim line) 0 2 with
           | "Kd" ->
             kd := parse_color line "Kd";
             read_lines ()
           | "Ks" ->
             ks := parse_color line "Ks";
             read_lines ()
           | "Ka" ->
             ka := parse_color line "Ka";
             read_lines ()
           | "ma" when String.starts_with ~prefix:"map_Kd" line ->
             texture_file := Scanf.sscanf line "map_Kd %s" (fun s -> s);
             read_lines ()
           | _ -> read_lines ())
        | _ -> read_lines ()
      ) else
        read_lines ()
    with
    | End_of_file -> ()
  in
  try
    read_lines ();
    close_in ic;
    !kd, !ks, !ka
  with
  | e ->
    close_in ic;
    raise e
;;

let read_obj_file filename =
  let vertices = ref [] in
  let normals = ref [] in
  let faces = ref [] in
  let textures = ref [] in
  let mtl_file = ref "" in
  let ic = open_in filename in
  try
    while true do
      let line = input_line ic in
      if String.length line >= 2 then (
        match String.sub line 0 2 with
        | "v " -> vertices := !vertices @ [ parse_vertex line ]
        | "vn" -> normals := !normals @ [ parse_normal line ]
        | "f " -> faces := !faces @ [ parse_face line ]
        | "vt" -> textures := !textures @ [ parse_texture line ]
        | "mt" -> mtl_file := parse_mtl_file line
        | "us" -> ()
        | _ -> ()
      )
    done;
    let kd, ks, ka = read_mtl_file !mtl_file in
    !vertices, !normals, !faces, !textures, kd, ks, ka
  with
  | End_of_file ->
    close_in ic;
    let kd, ks, ka = read_mtl_file !mtl_file in
    !vertices, !normals, !faces, !textures, kd, ks, ka
;;

let convert_to_scene (vertexs, normals, faces, textures, kd, ks, ka) =
  let convert_vertex v = Geometry.Point.from_coords v.x v.y v.z in
  let convert_normal n = Geometry.Direction.from_coords n.nx n.ny n.nz in
  (* Printf.printf
     "Converting %d vertexs and %d faces\n"
     (List.length vertexs)
     (List.length faces); *)
  let convert_face { v1 = p1, t1, n1; v2 = p2, t2, n2; v3 = p3, t3, n3 } =
    let v1 = List.nth vertexs (p1 - 1) in
    let v2 = List.nth vertexs (p2 - 1) in
    let v3 = List.nth vertexs (p3 - 1) in
    let n1 = List.nth normals (n1 - 1) in
    let n2 = List.nth normals (n2 - 1) in
    let n3 = List.nth normals (n3 - 1) in
    let t1 = List.nth textures (t1 - 1) in
    let t2 = List.nth textures (t2 - 1) in
    let t3 = List.nth textures (t3 - 1) in
    Scene.Figures.Figure
      (Scene.Figures.triangle
         { point = convert_vertex v1; normal = convert_normal n1; material = t1 }
         { point = convert_vertex v2; normal = convert_normal n2; material = t2 }
         { point = convert_vertex v3; normal = convert_normal n3; material = t3 }
         { emission = Rgb.zero ()
         ; coefficients =
             kd, ks, ka
             (* ; coefficients = Rgb.zero (), Rgb.zero (), Rgb.rgb_of_values 0.8 0.8 0.8 *)
         ; refraction = 0.66
         })
  in
  List.map convert_face faces
;;
