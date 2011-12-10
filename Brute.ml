open Problem;;

let p1 = Problem.make 4 13;;
let p2 = Problem.make 16 62;;
Problem.print_problem p1;;
let s = Problem.solve p1;;
List.iter (fun e-> if e then print_string "  +" else print_string "  -") s;
print_newline ();;
