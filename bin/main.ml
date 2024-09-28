open Computer_gfx.Geometry

let ppm_version = ref ""

let max = ref 0
let width, height = ref 0, ref 0
let ppm_max = ref 0

type rgb = { red: int64; green: int64; blue: int64 }

let tone_map (color: rgb) = { red = 0L; green = 0L; blue= 0L }

  

let read_values ic = 
  let rec read_next colors remaining_lines = 
    match remaining_lines with 
    | 0 -> colors
    | n -> begin
      let line = input_line ic in
      String.s

  in
  read_next [] !height

  

let read_ppm file = 
  let ic = open_in file in
  let rec read_header rem = 
    let line = input_line ic in
    match rem, String.sub line 0 1 with
    | _, "#"  -> begin
      match String.sub line 1 4 with
      | "MAX" -> max := int_of_string (String.sub line 5 (String.length line - 5)); read_header rem
      | _ -> read_header rem
    end
    | 3, _ -> ppm_version := line; read_header (rem-1)
    | 2, _ -> let w, h = Scanf.sscanf line "%d %d" (fun w h -> w, h) in width := w; height := h; read_header (rem-1)
    | 1, _ -> ppm_max := int_of_string line; read_header (rem-1)
    | 0, _ -> read_values ic
    | _, _ -> failwith "Invalid PPM file"
  in read_header 3
in
       

let test_tonemap () =
  (* Iter.(IO.chunks_of "forest_path.ppm" |> filter (fun l -> l <> "") |> IO.write_lines "forest_path_out.ppm") *)
  let ppm_colors: rgb list = read_ppm "forest_path.ppm" in
  List.map tone_map ppm_colors |> Iter.IO.write_lines "forest_path_out.ppm"

in

let real_tonemap_test file = 
  let ic = open_in file in
  let (ic, header) = Computer_gfx.Ppm.read_header ic in
  (* Buffer para ir rellenando con los resultados de aplicar tonemapping a los píxeles *)
  (* Cuando el buffer tenga length x -> volcar al fichero de salida *)
  let rec traverse_file buf in_chan = 
    match Computer_gfx.Ppm.read_pixel in_chan header with
    | None -> () (* Write buf to file *)
    | Some (ch, p) -> begin
      if List.length buf >= header.width then () (* Flush buf to file *);
      traverse_file (buf @ [tone_map p]) ch (* Dejar en este orden para que sea tail recursive *)
    end
  in
  traverse_file [] ic

let () = test_tonemap ()
