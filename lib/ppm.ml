type config = {
    ppm_version: string;
    max: float;
    ppm_max: int;
    width: int;
    height: int;
}

let read_header ic = 
  (* Printf.printf "Initial position -> %d" (pos_in ic);
  Printf.printf "First char -> %c" (input_char ic); *)
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
  


let load_pixel (p : Image.pixel) (conf : config) : Image.pixel = { 
  red = p.red *. conf.max /. float_of_int conf.ppm_max; 
  green = p.green *. conf.max /. float_of_int conf.ppm_max; 
  blue = p.blue *. conf.max /. float_of_int conf.ppm_max
}

let save_pixel (p : Image.pixel) (conf : config) : Image.pixel = { 
  red = p.red *. float_of_int conf.ppm_max /. conf.max; 
  green = p.green *. float_of_int conf.ppm_max /. conf.max;
  blue = p.blue *. float_of_int conf.ppm_max /. conf.max
}

let read_pixel ic conf =  
  try
    let red = read_number ic in
    let green = read_number ic in
    let blue = read_number ic in
    Some(load_pixel {red;green;blue} conf)
  with
  | End_of_file -> None
  | Failure err -> print_endline err; None
  | Scanf.Scan_failure err -> print_endline err; None

let write_header oc header = 
  output_string oc (Printf.sprintf "%s\n" header.ppm_version);
  output_string oc (Printf.sprintf "#MAX=%f\n" header.max);
  output_string oc (Printf.sprintf "%d %d\n" header.width header.height);
  output_string oc (Printf.sprintf "%d\n" header.ppm_max);
  oc

let write_pixel oc conf p =
  let ppm_pixel = save_pixel p conf in
  output_string oc (Printf.sprintf "%d %d %d     " (int_of_float ppm_pixel.red) (int_of_float ppm_pixel.green) (int_of_float ppm_pixel.blue))
