let square v = v *. v

type texture_map =
  { width : int
  ; height : int
  ; data : Cairo.Image.data32
  }
