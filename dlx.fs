require myiter.fs
require dlx_base.fs  
require dlx_utils.fs 

  
\ ------------------------------- Construction ---------------------------------
  

: init_node ( col idx n -- n )
  tuck .row_idx_field ! ( col idx n -- col n )
  over .col_idx over .col_idx_field ! ( col n -- col n )
  tuck .column_field ! ( col n -- n )
  dup dup .left_field ! ( n -- n )
  dup dup .right_field ! ( n -- n )
  dup dup .up_field ! ( n -- n )
  dup dup .down_field ! ( n --  )
  ;
  
: init_column { root idx c -- c }
  root -1 c init_node drop \ init keeps addr on stack. gi?
  idx c .col_idx_field !
  0 c .length_field !
  c
  ;
  
: new_node ( col idx -- addr )
  node% %alloc init_node ;
  
: get_column ( dlx n -- addr )
  1+ column% %size * swap .root +
  ; 

\ with num columns
: init_dlx ( n -- )
    dup column% %size * dlx% rot + ( n a n*s+s )
    %alloc ( -- n addr ) \ hope that works out with alignment
    2dup .col_count_field ! ( -- n addr )
    0 over .row_count_field ! ( -- n addr )
    -1 over .root tuck ( -- n addr root -1 root )
    init_column rot ( -- addr root n )
    over column% %size + ( -- addr root n col )
    swap 0 ?do ( -- addr root col )
      over i rot ( -- addr root root i col )
      init_column ( addr root col )
      2dup insert_left ( addr root col )
      column% %size + ( addr root col )
    loop
    2drop    
  ;
  
\ Insert rows...

: add_cell ( head node -- )
  over .length_field increment
  insert_up
  ;

: add_row { dlx rid iter -- }
  iter 
  dup iter_next? if
    dlx over iter_next ( -- iter root cid )
    get_column ( -- iter col )
    dup rid new_node ( -- iter col fst )    
    tuck add_cell ( -- iter fst )
    begin
      over iter_next?
      while
      over iter_next ( -- iter fst cid )
      dlx swap get_column ( -- iter fst col )       
      dup rid new_node ( -- iter fst col nn )
      tuck add_cell ( -- iter fst nn )
      over swap insert_left
    repeat
  endif
  2drop
  dlx .row_count_field increment
  ;

\

: dlx_read_matrix { dlx arr n -- }
  n dlx .col_count /mod swap 
  0<> if s" odd number of elements" exception throw endif
  0 ?do
    dlx .col_count arr over i chars * + ( -- n src)
    bin_arr_to_positions ( -- l src)
    arr_cell_iter dlx i rot add_row 
  loop
  ;
  
  
\ ---------------------------------- Testing ---------------------------------- 
  

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
  cr dup .mat_col_count dup . ." cols " over .mat_row_count dup . ." rows" ( -- mat cols rows )
  over * swap init_dlx ( -- mat sz dlx )
  dup >r -rot ( -- dlx mat sz )
  dlx_read_matrix 
  r>
;

: build_solution_rows { mat rows arr1 -- arr2 rows rl }
  rows cells mem_alloc ( -- arr2 )
  rows 0 ?do
    mat arr1 i cells + 
    @ ( -- arr2 mat rid )
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
  
: process_solution2 ( len arr -- )
  test_matrix -rot
  build_solution_rows ( -- arr2 rows rl )
  validate_solution
  ;
  
: process_solution ( len arr -- )
  swap 0 ?do ( -- arr )
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
  drop
  ;

: testalg ( dlx -- )
  ['] process_solution2 dlx_solve
;
               



  
\ --------------------

: tarr ( n -- )
  here over cells allot ( -- n addr )
  swap
  0 ?do ( -- addr )
    i over over  ( -- addr i addr i )
    cells + !
  loop
  ;
  
: hello
  ." hello" cr ;
  
hello
