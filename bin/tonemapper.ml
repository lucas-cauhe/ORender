
open Computer_gfx.Colorspace

module PpmDb = Computer_gfx.Db.Ppm

let tone_map_file in_file out_file = 
  let module RgbToneMapper = Computer_gfx.Tonemap.ToneMapper(Rgb) in
  let ic = open_in in_file in
  let oc = open_out out_file in
  let header = PpmDb.read_header ic in
  let out_conf : PpmDb.config = PpmDb.config_of_values "P3" (PpmDb.config_max header) 65535 (PpmDb.config_width header) (PpmDb.config_height header)  in
  PpmDb.write_header oc out_conf;
  
  let rec traverse_file chan_len = 
    match PpmDb.read_pixel ic header with
    | None -> close_out oc; close_in ic
    | Some (p) -> begin
      let chan_len = match chan_len with 
      | l when l == (PpmDb.config_width header) -> output_string oc "\n"; 0
      | l -> l
      in
      RgbToneMapper.tone_map p (RgbToneMapper.gamma_clamp (PpmDb.config_max header) (1./.4.) 200.) |> PpmDb.write_pixel oc out_conf;
      traverse_file (chan_len + 1)
    end
  in
  traverse_file 0

let () = tone_map_file "ppms/hdr/mpi_office.ppm" "ppms/ascii/mpi_office.ppm"

