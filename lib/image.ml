module type ToneMapper = sig
  val clamp : ?th:float -> float -> float
  
  val equalization : float -> float -> float
  
  val gamma : float -> float -> float -> float
  
  val gamma_clamp : float -> float -> float -> float -> float

end

module Image(Pixel: Colorspace.ColorSpace) : ToneMapper = struct
  type pixel = Pixel.pixel

  let tone_map p f = 
    let l_in = Pixel.luminance p in
    let l_out = f l_in in
    Pixel.merge_chans p l_in l_out

  let clamp ?(th = 1.) l_in = if l_in > th then th else l_in

  let equalization max l_in  = l_in /. (Pixel.luminance (Pixel.equalized max))

  (* Usage: gamma p header.max (1./.4.) *)
  let gamma k g l_in = if l_in > k then k else (BatFloat.pow l_in g) /. (BatFloat.pow k g)

  (* Usage: gamma_clamp p header.max (1./.4.) 3000. *)
  let gamma_clamp k g th l_in = gamma l_in k g |> clamp ~th:th
end


