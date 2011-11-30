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
              | (h1::t1, h2::t2) -> plus_help t1 t2 ((Field.plus h1 h2)::sum)
            in List.rev (plus_help v1 v2 [])

          let times s v =
            let rec times_help u prod = match u with
              | [] -> prod
              | hd::tl -> times_help tl ((Field.multiply s hd)::prod)
            in List.rev (times_help v [])

          let multiply v1 v2 =
            let rec mult_help u1 u2 sum = match (u1,u2) with
              | ([], []) -> sum
              | (_, []) | ([], _) -> raise Size_mismatch
              | (h1::t1, h2::t2) -> mult_help t1 t2 (Field.plus h1 h2)
            in mult_help v1 v2 Field.zero
                 
          let print_scalar e = Field.print e
          let print_vector v =
            let rec print_help v = match v with
              | [] -> print_string "]"
              | e::[] -> print_scalar e ; print_help []
              | e::tl -> print_scalar e ; print_string " " ; print_help tl
            in print_string "[" ; print_help v
        end

    module Matrix =
      functor (Field : FIELD) ->
        struct
          type element = Field.field
          module FieldVector = Vector(Field)
          type matrix = {array : FieldVector.vector list; row_major : bool}

          exception Dim_mismatch
        end
          
  end
