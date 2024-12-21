let square v = v *. v

type texture_map =
  { width : int
  ; height : int
  ; stride : int
  ; data : Cairo.Image.data32
  }
