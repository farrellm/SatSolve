module Real =
  struct
    type field = float

    let plus a b = a +. b
    let negative a = -.a
    let zero = 0.

    let multiply a b = a *. b
    let inverse a = 1. /. a
    let one = 1.

    let print a = print_float a
  end
