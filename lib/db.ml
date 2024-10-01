module type Db = sig
  type config
  type pixel
  
  (**
    Reads a ppm header from [in_channel] and returns a tuple that contains:
    - the input channel resulting of reading that header
    - the config read from the header
*)
  val read_header : in_channel -> config

  (**
    Reads from [in_channel] 3 values representing a pixel's colors and advances
    the "file offset" to the next pixel.
    Returns [None] if EOF is reached
  *)
  val read_pixel : in_channel -> config -> pixel option

  val write_header : out_channel -> config -> unit

  val write_pixel : out_channel -> config -> pixel -> unit

end

let rescale_pixel (p : Colorspace.rgb_pixel) (base : float) (scale : float) : Colorspace.rgb_pixel = { 
  red = p.red *. scale /. base; 
  green = p.green *. scale /. base; 
  blue = p.blue *. scale /. base
}


module Ppm: Db with type pixel := Colorspace.rgb_pixel = struct
  type config = {
    ppm_version: string;
    max: float;
    ppm_max: int;
    width: int;
    height: int;
  }

  let rec read_to_next_pixel ic = 
    match input_char ic with
    | c when c >= '0' && c <= '9' -> seek_in ic (pos_in ic - 1)
    | _ -> read_to_next_pixel ic

  let read_number ic =
    let rec read_digits acc =
      let c = input_char ic in
      if c >= '0' && c <= '9' then
        read_digits (acc * 10 + (int_of_char c - int_of_char '0'))
      else
        float_of_int acc
    in
    let result = read_digits 0 in
    read_to_next_pixel ic;
    result

  let read_header ic = 
    let rec header_reader rem config =
      try
        let line = input_line ic in
        match rem, String.sub line 0 1 with
        | _, "#"  -> begin
          match String.sub line 1 3 with
          | "MAX" -> header_reader rem {config with max = float_of_string (String.trim (String.sub line 5 (String.length line - 5)))}
          | _ -> header_reader rem config
        end
        | 3, _ -> header_reader (rem-1) {config with ppm_version = line}
        | 2, _ -> header_reader (rem-1) (Scanf.sscanf line "%d %d" (fun w h -> { config with width = w; height = h }))
        | 1, _ -> {config with ppm_max = int_of_string (String.trim line)}
        | _, _ -> failwith "Invalid PPM file"
      with
      | End_of_file -> Printf.printf "rem -> %d\n" rem; config
    in
  header_reader 3 {ppm_version = "654"; max = 1.; ppm_max = 1; width = 10; height = 10;}

  let read_pixel ic conf =  
    try
      let red = read_number ic in
      let green = read_number ic in
      let blue = read_number ic in
      Some(rescale_pixel {red;green;blue} (float_of_int conf.ppm_max) conf.max)
    with
    | End_of_file -> None
    | Failure err -> print_endline err; None
    | Scanf.Scan_failure err -> print_endline err; None

  let write_header oc header = 
    let header_string = Printf.sprintf "%s\n#MAX=%f\n%d %d\n%d\n" 
      header.ppm_version
      header.max
      header.width header.height
      header.ppm_max in
    print_endline header_string;
    output_string oc header_string

  let write_pixel oc conf p =
    let ppm_pixel = rescale_pixel p conf.max (float_of_int conf.ppm_max) in
    output_string oc (Printf.sprintf "%d %d %d     " (int_of_float ppm_pixel.red) (int_of_float ppm_pixel.green) (int_of_float ppm_pixel.blue))

  let config_of_values v max ppm_max w h = {ppm_version = v; max = max; ppm_max = ppm_max; width = w; height = h}
  let config_max c = c.max
  let config_width c = c.width
  let config_height c = c.height
end


