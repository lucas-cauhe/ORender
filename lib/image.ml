type pixel = {
    red: float;
    green: float;
    blue: float;
}

let luminance (p : pixel) : float = 0.2126 *. p.red +. 0.7152 *. p.green +. 0.0722 *. p.blue

let merge_chans (p : pixel) (l_in : float) (l_out : float) : pixel = { red = p.red *. (l_out /. l_in); green = p.green *. (l_out /. l_in); blue = p.blue *. (l_out /. l_in) }

let clamp (p : pixel)  : pixel = 
  let l_in = luminance p in
  let tone_mapped_l = if l_in > 1. then 1. else l_in in
  (* Printf.printf "Lin -> %f | Lout -> %f | Pixel -> r=%f,g=%f,b=%f\n" l_in tone_mapped_l p.red p.green p.blue; *)
  (* let tone_mapped_l = l_in *. (1. +. l_in /. 1_000.*.1_000.) /. (1. +. l_in) in *)
  merge_chans p l_in tone_mapped_l
let equalization (p : pixel) (max : float) : pixel = 
  let l_in = luminance p in
  let tone_mapped_l = l_in /. (luminance { red = max; green = max; blue = max }) in
  merge_chans p l_in tone_mapped_l

(* Usage: gamma p header.max (1./.4.) *)
let gamma (p : pixel) (k : float) (gamma: float) : pixel = 
  let l_in = luminance p in
  let tone_mapped_l = if l_in > k then k else (BatFloat.pow l_in gamma) /. (BatFloat.pow k gamma) in
  merge_chans p l_in tone_mapped_l


(* Usage: gamma_clamp p header.max (1./.4.) 3000. *)
let gamma_clamp (p : pixel) (k : float) (gamma: float) (v: float) : pixel = 
  let l_in = luminance p in
  let gamma_l = if l_in > k then k else (BatFloat.pow l_in gamma) /. (BatFloat.pow k gamma) in
  let tone_mapped_l = if gamma_l > v then v else gamma_l in
  merge_chans p l_in tone_mapped_l
