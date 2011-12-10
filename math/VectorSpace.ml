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

          exception Empty_column

          let l_divide m v =
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
           let partial = eliminate (List.combine m v) [] 0 1 in
           let full = eliminate partial [] ((List.length partial) - 1) (-1) in
           snd (List.split full)
        end
          
  end
