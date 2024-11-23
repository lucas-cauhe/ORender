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
  { v1 : int
  ; v2 : int
  ; v3 : int
  }

let parse_vertex line = Scanf.sscanf line "v %f %f %f" (fun x y z -> { x; y; z })
let parse_normal line = Scanf.sscanf line "vn %f %f %f" (fun nx ny nz -> { nx; ny; nz })

let parse_face line =
  Scanf.sscanf line "f %d/%d/%d %d/%d/%d %d/%d/%d" (fun v1 _ _ v2 _ _ v3 _ _ ->
    { v1; v2; v3 })
;;

let read_obj_file filename =
  let vertices = ref [] in
  let normals = ref [] in
  let faces = ref [] in
  let ic = open_in filename in
  try
    while true do
      let line = input_line ic in
      if String.length line >= 2 then (
        match String.sub line 0 2 with
        | "v " -> vertices := parse_vertex line :: !vertices
        | "vn" -> normals := parse_normal line :: !normals
        | "f " -> faces := parse_face line :: !faces
        | _ -> ()
      )
    done;
    !vertices, !normals, !faces
  with
  | End_of_file ->
    close_in ic;
    !vertices, !normals, !faces
;;

let convert_to_scene (vertexs, faces) =
  let convert_vertex v = Geometry.Point.from_coords v.x v.y v.z in
  (* Printf.printf
     "Converting %d vertexs and %d faces\n"
     (List.length vertexs)
     (List.length faces); *)
  let convert_face f =
    let v1 = List.nth vertexs (f.v1 - 1) in
    let v2 = List.nth vertexs (f.v2 - 1) in
    let v3 = List.nth vertexs (f.v3 - 1) in
    Scene.Figures.Figure
      (Scene.Figures.triangle
         (convert_vertex v1)
         (convert_vertex v2)
         (convert_vertex v3)
         { emission = Rgb.rgb_of_values 0.8 0.4 0.05
         ; coefficients = Rgb.rgb_of_values 0.75 0.75 0.75, Rgb.zero (), Rgb.zero ()
         ; refraction = 1.5
         }
       |> Option.get)
  in
  List.map convert_face faces
;;
