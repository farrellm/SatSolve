open Field;;

module VectorSpace =
  struct
    module Vector =
      functor (Field : FIELD) ->
        struct
          type element = Field.field
          type vector = element list

          exception Size_mismatch
                 
          let print_scalar e = Field.print e
          let print_vector v =
            let rec print_help v = match v with
              | [] -> print_string "]"
              | e::[] -> print_scalar e ; print_help []
              | e::tl -> print_scalar e ; print_string " " ; print_help tl in
            print_string "[" ; print_help v
          
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
        end

    module Matrix =
      functor (Field : FIELD) ->
        struct
          type element = Field.field
          module FieldVector = Vector(Field)
          type matrix = {array : FieldVector.vector list; row_major : bool}

          exception Dim_mismatch

          let print_scalar = FieldVector.print_scalar
          let print_vector = FieldVector.print_vector
          let print_matrix m =
            let rec print_help n = match n with
              | [] -> print_string "]"
              | v::[] -> print_vector v ; print_help []
              | v::tl -> print_vector v ;
                  print_newline () ; print_string " " ; print_help tl in
            print_string "[" ; print_help m

          let eye n =
            let rec row_loop r =
              if r==n then [] else
                let rec col_loop c =
                  if c==n then [] else
                    let e = if r==c then Field.one else Field.zero in
                    e :: (col_loop (c+1)) in
                (col_loop 0) :: (row_loop (r+1)) in
            row_loop 0
 
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

          exception Zero_vector

          let l_divide m v =
            let rec first_nonzero u n = match u with
              | [] -> raise Zero_vector
              | hd::tl ->
                if Field.is_zero hd then first_nonzero tl (n+1) else hd,n in
            let rec div_help n u res_n res_u = match n,u with
              | [],[] -> res_n,res_u
              | _,[] | [],_ -> raise Dim_mismatch
              | hd_n::tl_n , hd_u::tl_u ->
                  let e,i = first_nonzero hd_n 0 in
                  let inv = Field.inverse e in
                  let i_hd_n = FieldVector.times inv hd_n
                  and i_hd_u = FieldVector.times inv hd_u in
                  let rec eliminate o w res_o res_w = match o,w with
                    | [],[] -> (List.rev res_o),(List.rev res_w)
                    | _,[] | [],_ -> raise Dim_mismatch
                    | hd_o::tl_o , hd_w::tl_w ->
                        let a = List.nth hd_o i in
                        let mult = Field.negative a in
                        let row = FieldVector.times mult i_hd_n
                        and b = FieldVector.times mult i_hd_u in
                        let clr_o = FieldVector.plus hd_o row
                        and clr_w = FieldVector.plus hd_w b in
                        eliminate tl_o tl_w (clr_o::res_o) (clr_w::res_w) in
                  let f,g = eliminate tl_n tl_u [] [] in
                  div_help f g (i_hd_n::res_n) (i_hd_u::res_u) in
            let m_2,v_2 = div_help m v [] [] in
            div_help m_2 v_2 [] []
        end
          
  end
