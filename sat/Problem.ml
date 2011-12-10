open Set
open Random

module Problem =
  struct
    type literal = int * bool
        
    module OrderedInteger =
      struct
        type t = int
        let compare x y = x-y
      end
        
    module OrderedLiteral =
      struct
        type t = literal
        let compare x y =
          if (fst x) != (fst y) then (fst x) - (fst y) else
          if (snd x) == (snd y) then 0 else
          if snd x then 1 else -1
      end
        
    module IntegerSet = Set.Make(OrderedInteger)
    module LiteralSet = Set.Make(OrderedLiteral)
    module ClauseSet = Set.Make(LiteralSet)

    type problem = {clauses:ClauseSet.t ; nvar:int}
      
    exception Unsatisfiable

    let print_literal l =
      print_string (if not (snd l) then " -" else "  ") ; print_int (fst l)
    let print_clause c = LiteralSet.iter print_literal c ; print_newline ()
    let print_problem p = ClauseSet.iter print_clause p.clauses ; print_newline()

    let make nvar nclause =
      let rec make_loop clauses =
        if (ClauseSet.cardinal clauses) == nclause then clauses else
          let rec vars v =
            if (IntegerSet.cardinal v) == 3 then v else
              vars (IntegerSet.add (Random.int nvar) v) in
          let literals =
            IntegerSet.fold
              (fun e a->LiteralSet.add (e,Random.bool ()) a)
              (vars IntegerSet.empty) LiteralSet.empty in
          make_loop (ClauseSet.add literals clauses) in
      {clauses=(make_loop ClauseSet.empty) ; nvar=nvar}

    let solve p =
      let rec solve_rec sp i =
        if i==p.nvar then
          if (ClauseSet.cardinal sp)>0 then raise Unsatisfiable else
            []
        else
          let c,r = ClauseSet.partition
            (fun e -> LiteralSet.exists (fun f->(fst f) == i) e) sp in
          let s,u = ClauseSet.partition
            (fun e -> LiteralSet.exists (fun f->(fst f) == i && (snd f)) e) c in
          try
            true::solve_rec (ClauseSet.union u r) (i+1)
          with Unsatisfiable ->
            false::solve_rec (ClauseSet.union s r) (i+1) in
      solve_rec p.clauses 0

    let fast_solve p =
      let n = p.nvar * (p.nvar-1) * (p.nvar-2) / 6 in
      let index c1 c2 c3 = c1 + n*c2 + n*n*c3 in
  end
