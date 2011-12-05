open Real;;
open Rational;;
open VectorSpace;;

module RealVector = VectorSpace.Vector (Real) ;;
module RealMatrix = VectorSpace.Matrix (Real) ;;

module RationalVector = VectorSpace.Vector (Rational) ;;
module RationalMatrix = VectorSpace.Matrix (Rational) ;;

print_endline "SatSolver";;
(*
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

let row = RealMatrix.row [1.;0.];;
let col = RealMatrix.col [1.;0.];;

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
*)

let e2 = RealMatrix.eye 3;;
RealMatrix.print_matrix e2 ; print_newline () ;;

let m2 = [[1.;1.;2.];[2.;4.;-3.];[3.;6.;-5.]];;
RealMatrix.print_matrix m2;;
print_newline () ;;
let b2 = RealMatrix.col [1.;1.;1.];;
RealMatrix.print_matrix b2 ; print_newline () ; print_newline () ;;

match RealMatrix.l_divide m2 e2 with
  | a,b -> RealMatrix.print_matrix a ; print_newline () ; 
      RealMatrix.print_matrix b ; print_newline () ;;

let e3 = RationalMatrix.eye 3;;
RationalMatrix.print_matrix e3 ; print_newline () ;;

let m3 = [[1,1;1,1;2,1];[2,1;4,1;-3,1];[3,1;6,1;-5,1]];;
RationalMatrix.print_matrix m3;;
print_newline () ;;
let b3 = RationalMatrix.col [1,1;1,1;1,1];;
RationalMatrix.print_matrix b3 ; print_newline () ; print_newline () ;;

match RationalMatrix.l_divide m3 e3 with
  | a,b -> RationalMatrix.print_matrix a ; print_newline () ; 
      RationalMatrix.print_matrix b ; print_newline () ;;

print_newline () ;;
