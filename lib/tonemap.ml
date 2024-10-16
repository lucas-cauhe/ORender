module ToneMapper (CS: Colorspace.ColorSpace) = struct
  let tone_map p f = 
    let l_in = CS.luminance p in
    let l_out = f l_in in
    CS.merge_chans p l_in l_out

  let clamp ?(th = 1.) l_in = if l_in > th then th else l_in

  let equalization max l_in  = l_in /. (CS.luminance (CS.equalized max))

  (* Usage: gamma p header.max (1./.4.) *)
  let gamma k g l_in = if l_in > k then k else (BatFloat.pow l_in g) /. (BatFloat.pow k g)

  (* Usage: gamma_clamp p header.max (1./.4.) 3000. *)
  let gamma_clamp k g th l_in = gamma l_in k g |> clamp ~th:th
end
