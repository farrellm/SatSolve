open Real;;
open VectorSpace;;

module RealVector = VectorSpace.Vector (Real) ;;
module RealMatrix = VectorSpace.Matrix (Real) ;;

print_endline "SatSolver";;

let v1 = [1.;2.;3.];;
let v2 = [1.;0.;1.];;

RealVector.print_vector v1;;
print_newline () ;;
RealVector.print_vector v2;;
print_newline () ;;

let sum = RealVector.plus v1 v2;;
RealVector.print_vector sum;;
print_newline () ;;

let times = RealVector.times 2. v1;;
RealVector.print_vector times;;
print_newline () ;;

let dot = RealVector.multiply v1 v2;;
RealVector.print_scalar dot;;
print_newline () ;;

let m1 = [[1.;3.];[2.;5.]];;
RealMatrix.print_matrix m1;;
print_newline () ;;

print_newline () ;;
(*
let mt = RealMatrix.times 3. m1;;
RealMatrix.print_matrix mt ;;
print_newline () ;;
*)

let row = RealMatrix.row [1.;0.];;
let col = RealMatrix.col [1.;0.];;

(*
match RealMatrix.transpose col with
  | a,b -> RealMatrix.print_matrix a ; print_newline () ; 
      RealMatrix.print_vector b ; print_newline () ;;
*)
RealMatrix.print_matrix (RealMatrix.transpose m1) ;;
print_newline () ;;
print_newline () ;;

RealMatrix.print_matrix (RealMatrix.multiply m1 m1) ;;
print_newline () ;;
print_newline () ;;

RealMatrix.print_matrix row;;
print_newline () ;;

RealMatrix.print_matrix col;;
print_newline () ;;

print_newline () ;;
