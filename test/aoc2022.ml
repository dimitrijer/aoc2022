open Base

let solve_or_throw day input =
  Stdio.print_endline
  @@ Or_error.ok_exn
  @@ Solutions.solve day
  @@ String.chop_prefix_exn ~prefix:"\n" input
;;

let%expect_test "day05" =
  solve_or_throw
    5
    {|
    [D]    
[N] [C]    
[Z] [M] [P]
 1   2   3 

move 1 from 2 to 1
move 3 from 1 to 3
move 2 from 2 to 1
move 1 from 1 to 2
|};
  [%expect {|
CMZ
MCD|}]
;;
