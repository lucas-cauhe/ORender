(* open Computer_gfx.Geometry *)  
open Computer_gfx.Image 

(* let test_tonemap () =
  (* Iter.(IO.chunks_of "forest_path.ppm" |> filter (fun l -> l <> "") |> IO.write_lines "forest_path_out.ppm") *)
  let ppm_colors: rgb list = read_ppm "forest_path.ppm" in
  List.map tone_map ppm_colors |> Iter.IO.write_lines "forest_path_out.ppm"

in *)

let test_tonemap in_file out_file = 
  let ic = open_in in_file in
  let oc = open_out out_file in
  let header = Computer_gfx.Ppm.read_header ic in
  let out_conf : Computer_gfx.Ppm.config = {ppm_version = "P3"; max = header.max; ppm_max = 65535; width = header.width; height = header.height} in
  let oc = Computer_gfx.Ppm.write_header oc out_conf in
  
  let rec traverse_file chan_len = 
    match Computer_gfx.Ppm.read_pixel ic header with
    | None -> close_out oc; close_in ic
    | Some (p) -> begin
      let chan_len = match chan_len with 
      | l when l == header.width -> output_string oc "\n"; flush oc; 0
      | l -> l
      in
      clamp p |> Computer_gfx.Ppm.write_pixel oc out_conf; (* Flush buf to file *)
      traverse_file (chan_len + 1)
    end
  in
  traverse_file 0
  
  
let () = test_tonemap "ppms/hdr/mpi_office.ppm" "ppms/ascii/mpi_office.ppm"

