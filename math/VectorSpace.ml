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
          let print_vector = function
            | [] -> print_string "[]"
            | hd::tl ->
                print_string "[" ; print_scalar hd ;
                List.iter (function e -> print_string " " ; print_scalar e) tl ;
                print_string "]"
          
          let plus v1 v2 = List.map2 Field.plus v1 v2

          let times s v = List.map (function e -> Field.multiply s e) v

          let multiply v1 v2 =
            List.fold_left2 (fun s a b -> Field.plus s (Field.multiply a b))
              Field.zero v1 v2
        end

    module Matrix =
      functor (Field : FIELD) ->
        struct
          type element = Field.field
          module FieldVector = Vector(Field)
          type matrix = FieldVector.vector list

          exception Dim_mismatch

          let print_scalar = FieldVector.print_scalar
          let print_vector = FieldVector.print_vector
          let print_matrix = function
            | [] -> print_string "[[]]"
            | hd::tl ->
                print_string "[" ; print_vector hd ;
                List.iter (function e -> print_newline () ; print_string " " ;
                             print_vector e ) tl ;
                print_string "]"

          let eye n =
            let rec row_loop r =
              if r==n then [] else
                let rec col_loop c =
                  if c==n then [] else
                    let e = if r==c then Field.one else Field.zero in
                    e :: (col_loop (c+1)) in
                (col_loop 0) :: (row_loop (r+1)) in
            row_loop 0
              
          (*
          type eye_rec = {before: matrix; after: matrix}
          let ey n =
            let rec init u = if (List.length u) == n then u else init ([]::u) in
            let push_zero v = Field.zero::v
            and push_one v = Field.one::v
            and base = init [] in
            (List.fold_left
               (fun p e-> match p.after with
                  | [] -> raise Dim_mismatch
                  | hd::tl -> {
                      before=(push_one hd)::(List.map push_zero p.before);
                      after=(List.map push_zero tl)})
               {before=[];after=base} base).before
          *)
              
          let row v = [v]
          let col v = List.map (function e -> [e]) v
                
          let transpose m =
            let chop n = List.fold_right
              (fun v p -> match v with
                 | [] -> [],[]
                 | hd::tl -> (hd::(fst p)),(tl::(snd p))) n ([],[]) in
            let rec trans_loop n =
              match chop n with
                | [],[] -> []
                | _,[] | [],_ -> raise Dim_mismatch
                | v,r -> v::(trans_loop r) in
            trans_loop m

          let times s m = List.map (function v -> FieldVector.times s v) m

          let multiply m1 m2 =
            let t2 = transpose m2 in
            List.map
              (function v1 -> List.map
                 (function v2 -> FieldVector.multiply v1 v2) t2) m1

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
            (*m_2,v_2*)

          exception Empty_column

          let divide m v =
           let init_aug = List.rev_map2 (fun a b -> a,b) m v in
           let rec eliminate aug res i di =
             let rec next_row a b = match a with
               | [] -> raise Empty_column
               | hd::tl -> match List.nth (fst hd) i with
                   | e when Field.is_zero e -> next_row tl (hd::b)
                   | _ -> hd,tl@b in
             if aug == [] then res else
               try
                 let (row,b),rest = next_row aug [] in
                 let inv = Field.inverse (List.nth row i) in
                 let irow = FieldVector.times inv row
                 and ib = FieldVector.times inv b in
                 let clear = function r,c ->
                   let mult = Field.negative (List.nth r i) in
                   (FieldVector.plus r (FieldVector.times mult irow)),
                   (FieldVector.plus c (FieldVector.times mult ib)) in
                 let next = List.map clear rest in
                   eliminate next ((irow,ib)::res) (i+di) di
               with Empty_column -> eliminate aug res (i+di) di in
           let partial = eliminate init_aug [] 0 1 in
           let full = eliminate partial [] ((List.length partial) - 1) (-1) in
           let n,u = List.fold_left
             (fun p a->((fst a)::(fst p)),((snd a)::(snd p)))
             ([],[]) (List.rev full) in
             n,u

        end
          
  end
