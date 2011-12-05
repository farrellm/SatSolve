module Rational =
  struct
    type field = int * int

    let rec gcd a b =
      if a>b then gcd b a else
        let m = b mod a in
          if m=0 then a else gcd m a

    let make n d =
      let g = gcd n d in
      (n/g),(d/g)

    let plus a b =
      make ((fst a)*(snd b) + (fst b)*(snd a)) ((snd a)*(snd b))
    let negative a = -(fst a),(snd a)
    let zero = 0,1
    let is_zero a = (fst a) == 0

    let multiply a b =
      make ((fst a)*(fst b)) ((snd a)*(snd b))
    let inverse a = if (fst a)>0 then (snd a),(fst a) else -(snd a),-(fst a)
    let one = 1,1

    let print a = print_int (fst a) ; print_string "/" ; print_int (snd a)
  end
