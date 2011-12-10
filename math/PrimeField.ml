module PrimeField =
  functor (T:sig val n : int end) ->
  struct
    type field = int

    let plus a b = (a + b) mod T.n
    let negative a = (T.n - a) mod T.n
    let zero = 0
    let is_zero a = a==0

    let multiply a b = (a * b) mod T.n
    let inverse a = 
      let rec inv_loop i = if (multiply a i)==1 then i else inv_loop (i+1) in
      inv_loop 0
    let one = 1

    (* let print a = print_int a ; print_string "_" ; print_int T.n *)
    let print a = print_int a
  end
