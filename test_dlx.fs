require dlx.fs

\ ---------------------------------- Testing ---------------------------------- 
  
create trow1 0 , 3 , 6 ,
create trow2 0 , 3 , 
create trow3 3 , 4 , 6 ,
create trow4 2 , 4 , 5 ,
create trow5 1 , 2 , 5 , 6 ,
create trow6 1 , 6 ,

: test1
  dlx_init
  dup 0 trow1 3 dlx_add_row 
  dup 1 trow2 2 dlx_add_row 
  dup 2 trow3 3 dlx_add_row 
  dup 3 trow4 3 dlx_add_row 
  dup 4 trow5 4 dlx_add_row 
  dup 5 trow6 2 dlx_add_row 
  ;

\ 1, 0, 0, 1, 0, 0, 1 
\ 1, 0, 0, 1, 0, 0, 0 
\ 0, 0, 0, 1, 1, 0, 1 
\ 0, 0, 1, 0, 1, 1, 0 
\ 0, 1, 1, 0, 0, 1, 1
\ 0, 1, 0, 0, 0, 0, 1 

6 7 
matrix[ 1 c, 0 c, 0 c, 1 c, 0 c, 0 c, 1 c,
        1 c, 0 c, 0 c, 1 c, 0 c, 0 c, 0 c,
        0 c, 0 c, 0 c, 1 c, 1 c, 0 c, 1 c,
        0 c, 0 c, 1 c, 0 c, 1 c, 1 c, 0 c,
        0 c, 1 c, 1 c, 0 c, 0 c, 1 c, 1 c,
        0 c, 1 c, 0 c, 0 c, 0 c, 0 c, 1 c, 
]matrix mat_var test_matrix1
             
variable test_vmatrix
test_matrix1 test_vmatrix !

: test_matrix
  test_vmatrix @ ;


: load_test_matrix ( "name" -- )
  require
  test_vmatrix !
  ;

: testdlx ( mat -- dlx )
  cr dup .mat_col_count . ." cols " dup .mat_row_count . ." rows" ( -- mat )
  dlx_init ( -- mat dlx )
  tuck swap dlx_read_matrix 
;

: build_solution_rows { mat rows arr1 -- arr2 rows rl }
  rows cells mem_alloc ( -- arr2 )
  rows 0 ?do
    mat arr1 i cells + @ ( -- arr2 mat rid )
    mat_get_row over i cells + !    
  loop
  rows mat .mat_col_count
  ;

: validate_solution { arr rows rl -- }
  here { sav }
  rl chars allot_empty ( -- buf )
  rows 0 ?do
    arr i cells + @ ( buf row )
    rl 0 ?do ( -- buf row )
      over i chars + tuck c@ ( -- buf boff row c )
\      dup .
      over c@ 
      dup . + ( -- buf boff row c )
      dup 1 > if s" same column twice" exception throw endif
      rot c! ( -- buf row )
    char+ loop
    drop
    cr
  loop
  cr
  rl 0 ?do ( -- buf )
    dup c@
    dup .
    1 <> if s" missing column " exception throw endif
    char+
  loop
  cr
  drop sav unallot_above
;
    
: process_solution2 ( arr len -- flag)
  test_matrix -rot swap
  build_solution_rows ( -- arr2 rows rl )
  validate_solution
  -1
  ;
  
: process_solution ( arr len -- flag )
  0 ?do ( -- arr )
    dup i cells + @ dup . ( -- arr rid )
    test_matrix tuck swap  ( -- arr m m rid )  
    mat_get_row swap .mat_col_count  ( -- arr row rl )
    0 ?do ( -- arr row )
      dup c@ .
      1 chars +
    loop
    drop
    cr
  loop
  drop -1
  ;

: testalg ( dlx -- )
  ['] process_solution2 dlx_solve
; 
