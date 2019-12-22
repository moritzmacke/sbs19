require dlx_base.fs  
require dlx_utils.fs 

  
\ ------------------------------- Construction ---------------------------------
  

: init_node ( col idx addr -- addr )
  tuck .row_idx_field ! ( col idx n -- col n )
  over .col_idx over .col_idx_field ! ( col n -- col n )
  tuck .column_field ! ( col n -- n )
  dup link_self
  ;
  
: init_column ( dlx idx addr -- addr )
  tuck .col_idx_field ! ( -- dlx addr )
  -1 over .row_idx_field !
  0 over .length_field !
  tuck .column_field ! ( -- addr ) \ store dlx for col instead ...
  dup link_self
  ;
  
: new_node ( col idx -- addr )
  node% %mem_alloc init_node ;
  
: new_column ( dlx idx -- addr )
  column% %mem_alloc init_column ;
    
1 constant initial_col_capacity
1 constant initial_row_capacity
  
\ empty
: dlx_init ( -- dlx )
    dlx% %mem_alloc ( -- addr ) 
    0 over .row_count_field !
    0 over .col_count_field !
    0 over .partial_solution_field !
    0 over .partial_solution_length_field !
    initial_col_capacity cells tuck over .col_array_capacity_field ! ( -- sz addr )
    swap mem_alloc over .col_array_field !
    initial_row_capacity cells tuck over .row_array_capacity_field !
    swap mem_alloc over .row_array_field ! ( -- addr )
    dup -1 over .root ( -- addr addr -1 root )
    init_column ( -- addr col )
    drop
  ;
  
: dlx_free ( dlx -- )
  
  ;
  
\ TODO
: reset_rows ( )
  ;
  
: reset_columns ( addr -- )

  ;
  
: dlx_reset ( dlx -- )

  ;
    
: resize_cols ( dlx cols -- )
  cells 2dup over 
  .col_array swap ( dlx sz dlx arr sz )
  mem_resize ( -- dlx sz dlx arr )
  swap .col_array_field ! ( -- dlx sz )
  swap .col_array_capacity_field !
  ;
  
  
: set_cols_upto ( dlx cid -- )
  1+ over .col_count ?do ( -- dlx)
    dup dup .root over i new_column ( -- dlx dlx root addr )
    tuck insert_left ( -- dlx dlx addr )
    i swap dlx_set_column ( -- dlx)
  loop drop 
  ;
  
\ add if not present
: set_column ( dlx cid -- )
  2dup swap .col_count >= if ( -- dlx cid )
    2dup 1+ resize_cols ( -- dlx cid )   
    2dup set_cols_upto ( -- dlx cid ) 
    1+ swap .col_count_field ! ( -- )
  else
    2drop
  endif
  ;
  
\ Insert rows...

: col_add_cell ( head node -- )
  over .length_field increment
  insert_up
  ;

\ dyn_resize ( addr n1 n2 -- addr n1/n3 )
  
: add_to_rows { dlx cel -- }
  dlx .row_array dlx .row_array_capacity 
  cel .row_idx 1+ cells dyn_resize ( -- addr sz )
  dlx .row_array_capacity_field !
  dup dlx .row_array_field ! ( -- addr )
  cel tuck .row_idx cells + !
  ;
  
  
: row_new_cell { dlx rid cid -- cel }
  dlx cid 2dup set_column dlx_get_column ( -- caddr )
  dup rid new_node ( -- caddr naddr )
  tuck col_add_cell
\    dup ." added " print_node_full
  ;
    
: dlx_add_row { dlx rname arr len -- }
  dlx dup 2dup .row_count ( -- dlx dlx dlx rid )
  arr @ row_new_cell { fst } ( -- dlx dlx )
  fst add_to_rows ( -- dlx )
  dup .row_count_field increment
  fst len 1 ?do ( -- dlx fst )
    2dup tuck .row_idx arr i cells + @ row_new_cell ( -- dlx fst fst cel )
    insert_left
  loop
  2drop
  ;

\

: dlx_read_matrix ( dlx mat -- )
  dup .mat_row_count
  0 ?do ( -- dlx mat )
    2dup dup i mat_get_row ( -- dlx mat dlx mat arr )
    swap .mat_col_count bin_arr_to_positions ( -- dlx mat dlx arr2 l)
    i -rot dlx_add_row 
  loop
  2drop
  ;
  
: dlx_read_compact ( dlx -- )
  
  ;
  
: dlx_set_partial_solution ( dlx addr len -- )
  rot tuck .partial_solution_length_field !     ( -- addr dlx)
  .partial_solution_field !
  ;
  
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
               
." included dlx.fs" cr
