(* open Computer_gfx.Geometry *)  
       

(* let test_tonemap () =
  (* Iter.(IO.chunks_of "forest_path.ppm" |> filter (fun l -> l <> "") |> IO.write_lines "forest_path_out.ppm") *)
  let ppm_colors: rgb list = read_ppm "forest_path.ppm" in
  List.map tone_map ppm_colors |> Iter.IO.write_lines "forest_path_out.ppm"

in *)

let luminance (p : Computer_gfx.Ppm.pixel) : float = 0.7152 *. p.red +. 0.2126 *. p.green +. 0.0722 *. p.blue

let _clamp (p : Computer_gfx.Ppm.pixel) : Computer_gfx.Ppm.pixel = 
  let l_in = luminance p in
  let tone_mapped_l = if l_in > 1. then 1. else l_in in
  (* let tone_mapped_l = luminance *. (1. +. luminance /. 1_000.*.1_000.) /. (1. +. luminance) in *)
  { red = p.red *. (tone_mapped_l /. l_in); green = p.green *. (tone_mapped_l /. l_in); blue = p.blue *. (tone_mapped_l /. l_in) }

let _equalization (p : Computer_gfx.Ppm.pixel) (max : float) : Computer_gfx.Ppm.pixel = 
  let l_in = luminance p in
  let tone_mapped_l = l_in /. (luminance { red = max; green = max; blue = max }) in
  { red = p.red *. (tone_mapped_l /. l_in); green = p.green *. (tone_mapped_l /. l_in); blue = p.blue *. (tone_mapped_l /. l_in) }

let gamma (p : Computer_gfx.Ppm.pixel) (k : float) (gamma: float) : Computer_gfx.Ppm.pixel = 
  let l_in = luminance p in
  let tone_mapped_l = if l_in > k then k else (BatFloat.pow l_in gamma) /. (BatFloat.pow k gamma) in
  { red = p.red *. (tone_mapped_l /. l_in); green = p.green *. (tone_mapped_l /. l_in); blue = p.blue *. (tone_mapped_l /. l_in) }

let _gamma_clamp (p : Computer_gfx.Ppm.pixel) (k : float) (gamma: float) (v: float) : Computer_gfx.Ppm.pixel = 
  let l_in = luminance p in
  let gamma_l = if l_in > k then k else (BatFloat.pow l_in gamma) /. (BatFloat.pow k gamma) in
  let tone_mapped_l = if gamma_l > v then v else gamma_l in
  { red = p.red *. (tone_mapped_l /. l_in); green = p.green *. (tone_mapped_l /. l_in); blue = p.blue *. (tone_mapped_l /. l_in) }

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
        traverse_file [gamma p header.max (1./.4.)] ch
      end
      else
        traverse_file (buf @ [gamma p header.max (1./.4.)]) ch (* Dejar en este orden para que sea tail recursive *)
    end
  in
  traverse_file [] ic

let () = test_tonemap "ppms/hdr/mpi_office.ppm" "ppms/ascii/mpi_office.ppm"

