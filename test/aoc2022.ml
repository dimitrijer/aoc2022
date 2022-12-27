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

let%expect_test "day07" =
  solve_or_throw
    7
    {|
$ cd /
$ ls
dir a
14848514 b.txt
8504156 c.dat
dir d
$ cd a
$ ls
dir e
29116 f
2557 g
62596 h.lst
$ cd e
$ ls
584 i
$ cd ..
$ cd ..
$ cd d
$ ls
4060174 j
8033020 d.log
5626152 d.ext
7214296 k
|};
  [%expect {|
95437
24933642|}]
;;
