open Field;;

module VectorSpace =
  struct
    module Vector =
      functor (Field : FIELD) ->
        struct
          type element = Field.field
          type vector = element list

          exception Size_mismatch
          
          let plus v1 v2 =
            let rec plus_help u1 u2 sum = match (u1,u2) with
              | ([], []) -> sum
              | (_, []) | ([], _) -> raise Size_mismatch
              | (h1::t1, h2::t2) -> plus_help t1 t2 ((Field.plus h1 h2)::sum) in
            List.rev (plus_help v1 v2 [])

          let times s v =
            let rec times_help u prod = match u with
              | [] -> prod
              | hd::tl -> times_help tl ((Field.multiply s hd)::prod) in
            List.rev (times_help v [])

          let multiply v1 v2 =
            let rec mult_help u1 u2 sum = match (u1,u2) with
              | ([], []) -> sum
              | (_, []) | ([], _) -> raise Size_mismatch
              | (h1::t1, h2::t2) ->
                  mult_help t1 t2 (Field.plus (Field.multiply h1 h2) sum) in
            mult_help v1 v2 Field.zero
                 
          let print_scalar e = Field.print e
          let print_vector v =
            let rec print_help v = match v with
              | [] -> print_string "]"
              | e::[] -> print_scalar e ; print_help []
              | e::tl -> print_scalar e ; print_string " " ; print_help tl in
            print_string "[" ; print_help v
        end

    module Matrix =
      functor (Field : FIELD) ->
        struct
          type element = Field.field
          module FieldVector = Vector(Field)
          type matrix = {array : FieldVector.vector list; row_major : bool}

          exception Dim_mismatch

          let row v = [v]
          let col v =
            let rec col_help u res = match u with
              | [] -> res
              | hd::tl -> col_help tl ([hd]::res) in
            List.rev (col_help v [])

          let transpose m =
            let rec next_vector s e v = match s with
              | [] -> (List.rev e), (List.rev v)
              | u::r -> match u with
                  | [] -> ([],[])
                  | hd::tl -> next_vector r (tl::e) (hd::v) in
            let rec trans_loop n r = match (next_vector n [] []) with
              | [],[] -> List.rev r
              | p,v -> trans_loop p (v::r) in
            trans_loop m []

          let times s m =
            let rec times_help u prod = match u with
              | [] -> prod
              | hd::tl -> times_help tl ((FieldVector.times s hd)::prod) in
            List.rev (times_help m [])

          let multiply m1 m2 =
            let t2 = transpose m2 in
            let rec out_loop n1 res = match n1 with
              | [] -> List.rev res
              | row::row_tl ->
                  let rec in_loop s1 v = match s1 with
                    | [] -> List.rev v
                    | col::col_tl ->
                        let prod = FieldVector.multiply row col in
                        in_loop col_tl (prod::v) in
                  let vec = in_loop t2 [] in
                  out_loop row_tl (vec::res) in
            out_loop m1 []

          let print_scalar = FieldVector.print_scalar
          let print_vector = FieldVector.print_vector
          let print_matrix m =
            let rec print_help n = match n with
              | [] -> print_string "]"
              | v::[] -> print_vector v ; print_help []
              | v::tl -> print_vector v ;
                  print_newline () ; print_string " " ; print_help tl in
            print_string "[" ; print_help m
        end
          
  end
