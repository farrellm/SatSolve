open Real;;
open VectorSpace;;

module RealVector = VectorSpace.Vector (Real) ;;

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

let m1=[[

print_newline () ;;
