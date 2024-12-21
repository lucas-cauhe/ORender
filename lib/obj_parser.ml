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
  Scanf.sscanf line "f %d/%d/%d %d/%d/%d %d/%d/%d" (fun p1 t1 n1 p2 t2 n2 p3 t3 n3 ->
    { v1 = p1, t1, n1; v2 = p2, t2, n2; v3 = p3, t3, n3 })
;;

let parse_texture line = Scanf.sscanf line "vt %f %f" (fun u v -> u, v)

let read_obj_file filename =
  let vertices = ref [] in
  let normals = ref [] in
  let faces = ref [] in
  let textures = ref [] in
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
        | _ -> ()
      )
    done;
    !vertices, !normals, !faces, !textures
  with
  | End_of_file ->
    close_in ic;
    !vertices, !normals, !faces, !textures
;;

let convert_to_scene (vertexs, normals, faces, textures) =
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
             Rgb.rgb_of_values 0.8 0.8 0.8, Rgb.zero (), Rgb.zero ()
             (* ; coefficients = Rgb.zero (), Rgb.zero (), Rgb.rgb_of_values 0.8 0.8 0.8 *)
         ; refraction = 0.66
         })
  in
  List.map convert_face faces
;;
