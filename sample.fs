require dlx.fs


7 10
matrix[
0 c, 1 c, 0 c, 1 c, 1 c, 0 c, 0 c, 1 c, 1 c, 0 c,
1 c, 0 c, 0 c, 0 c, 0 c, 1 c, 0 c, 0 c, 0 c, 0 c,
0 c, 0 c, 1 c, 0 c, 0 c, 0 c, 1 c, 0 c, 0 c, 1 c,
1 c, 0 c, 1 c, 0 c, 0 c, 0 c, 0 c, 0 c, 0 c, 1 c,
0 c, 0 c, 0 c, 0 c, 0 c, 1 c, 1 c, 0 c, 0 c, 0 c,
0 c, 1 c, 0 c, 1 c, 0 c, 0 c, 0 c, 0 c, 1 c, 0 c,
0 c, 0 c, 0 c, 0 c, 1 c, 0 c, 0 c, 1 c, 0 c, 0 c,
]matrix mat_var test_mat001

create test_sol001 1 , 5 ,


: process_solution ( len arr -- )
  swap 0 ?do ( -- arr )
    dup i cells + @ dup . ." :" ( -- arr rid )
    test_mat001 tuck swap  ( -- arr m m rid )  
    mat_get_row swap .mat_col_count  ( -- arr row rl )
    0 ?do ( -- arr row )
      dup c@ .
      1 chars +
    loop
    drop
    cr
  loop
  drop
  ;

: test
  dlx_init
  dup test_mat001 dlx_read_matrix
  ['] process_solution dlx_solve
  ;
  
: test_with_solution
  dlx_init
  dup test_mat001 dlx_read_matrix
  dup test_sol001 2 dlx_set_partial_solution 
  ['] process_solution dlx_solve
  ;
  
