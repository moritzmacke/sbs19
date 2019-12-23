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

: free_row ( cel -- )
  dup [ ' mem_free ] [for_row_except] 
  mem_free ;
  
: free_partial_solution ( dlx -- )
  dup .partial_solution_field dup @ 0<> if ( -- dlx addr )
    dup @ mem_free
  endif
  0 swap ! .partial_solution_length_field 0 swap ! 
  ;
  
: dlx_free ( dlx -- )
  dup dup .row_array swap .row_count [ ' free_row ] [@_arr_for_all]
  dup dup .col_array swap .col_count [ ' mem_free ] [@_arr_for_all]
  dup .row_array mem_free
  dup .col_array mem_free
  dup free_partial_solution
  dup dlx% %size 0 fill mem_free ;
  
\ TODO ?
  
: dlx_reset ( dlx -- )

  ;
  
\ Inserting columns
    
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

\ Reading in matrix of 1s and 0s

: dlx_read_matrix ( dlx mat -- )
  dup .mat_row_count
  0 ?do ( -- dlx mat )
    2dup dup i mat_get_row ( -- dlx mat dlx mat arr )
    swap .mat_col_count positional_from_bin_arr ( -- dlx mat dlx arr2 l)
    i -rot dlx_add_row 
  loop
  2drop
  ;
  
: dlx_read_compact ( dlx -- )
  
  ;
  
: copy_partial_solution ( addr len -- addr2 len )
  tuck cells dup mem_alloc ( -- len src sz dst )
  dup >r swap cmove r> swap ;
  
: dlx_set_partial_solution ( dlx addr len -- )
  rot dup free_partial_solution -rot
  copy_partial_solution ( -- dlx addr len )
  rot tuck .partial_solution_length_field !     ( -- addr dlx)
  .partial_solution_field !
  ;
               
." included dlx.fs" cr
