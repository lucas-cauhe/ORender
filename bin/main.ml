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
  let (ic, header) = Computer_gfx.Ppm.read_header ic in
  let out_conf : Computer_gfx.Ppm.config = {ppm_version = "P3"; max = header.max; ppm_max = 65535; width = header.width; height = header.height} in
  let () = Computer_gfx.Ppm.write_header oc out_conf in
  
  let rec traverse_file buf in_chan = 
    match Computer_gfx.Ppm.read_pixel in_chan header with
    | None -> Computer_gfx.Ppm.write_pixels oc out_conf buf  (* Write buf to file *)
    | Some (ch, p) -> begin
      if List.length buf >= header.width then begin
        Computer_gfx.Ppm.write_pixels oc out_conf buf; (* Flush buf to file *)
        output_string oc "\n";
        traverse_file [clamp p] ch
      end
      else
        traverse_file (buf @ [clamp p]) ch (* Dejar en este orden para que sea tail recursive *)
    end
  in
  traverse_file [] ic

let () = test_tonemap "ppms/hdr/mpi_office.ppm" "ppms/ascii/mpi_office.ppm"

