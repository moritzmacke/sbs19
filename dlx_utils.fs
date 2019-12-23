require dlx_base.fs 

\ --------------------------------------------
  
  
\ traverse linked list of nodes

: [apply_from_to] { nxt xt } ( runtime: end cur --  )  
  postpone begin
    postpone dup xt compile,
    postpone 2dup postpone <> postpone while
      nxt compile,
  postpone repeat 
  postpone 2drop ; immediate

: [apply_rgt_from_to] ( xt  -- ; runtime: end cur -- )
  ['] .right swap postpone [apply_from_to] ; immediate
  
: [apply_down_from_to] ( xt  -- ; runtime: end cur -- )
  ['] .down swap postpone [apply_from_to] ; immediate
  
: [for_row_except] ( xt -- ; runtime: cur -- )
  postpone dup postpone .left postpone swap postpone .right
  postpone [apply_rgt_from_to]
  ; immediate
  
: [for_col_except] ( xt -- ; runtime: cur -- )
  postpone dup postpone .up postpone swap postpone .down
  postpone [apply_down_from_to]
  ; immediate  
  
  
\ create array of positions of 1s in original

: positional_from_bin_arr ( arr n -- arr n )
  dup cells mem_alloc ( src n dst )
  dup >r swap 0 ?do ( src dst )
    over i chars + c@ ( src dst c )
    0<> if
      i over ! cell+
    endif
  loop
  nip r@ - r> swap ( -- arr sz )
  tuck mem_resize swap cell / ;
  
  
\ --------- Collect cells from whole matrix ----------
 
: node_is_head? ( addr -- flag )
  .row_idx -1 = ;
  
: node_is_root? ( addr -- flag )
  dup node_is_head? if
    .col_idx -1 =
  else 
    drop false
  endif
  ;
  
: all_cells_next ( cur -- nxt )
  begin
    .down dup node_is_head? if
      .right
    else exit endif
    dup node_is_root?
  until
  ;
  
\ ------------------------------- Printing etc --------------------------------

: print_node ( addr -- )
  ." (" dup .row_idx . ." ," .col_idx . ." )" ;
   
: print_node_full ( addr -- )
  cr dup print_node
  ." {c:" dup .column print_node
  ." , l:" dup .left print_node
  ." , r:" dup .right print_node
  ." , u:" dup .up print_node
  ." , d:" .down print_node
  ." }" ;
  
: print_column ( addr -- )
  cr dup print_node
  ." { l:" dup .left print_node
  ." , r:" dup .right print_node
  ." , len:" .length .
  ." }" ;

  
: print_col_array ( dlx -- )
  dup .col_array swap .col_count [ ' print_column ] [@_arr_for_all] ;
  
: print_row_array ( dlx -- )
  dup .row_array swap .row_count [ ' print_node ] [@_arr_for_all] ;
  
: print_active_cols
  .root [ ' print_node ] [for_row_except] ;
  
: print_col_nodes ( col -- )
  [ ' print_node ] [for_col_except] ;
  
: print_active_nodes ( dlx -- )
  .root [ ' print_col_nodes ] [for_row_except] ;
  
    
\ ----------- printing the sparse matrix -------------

: mark_row_active ( mat row -- )
  3 -rot 0 mat_set ;
  
: mark_col_active ( mat col -- )
  1+ 3 -rot over .mat_row_count 1- swap mat_set ;

: set_cell ( mat cel -- )
  1 -rot dup .row_idx swap .col_idx 1+ mat_set ;
  
: mark_active_cells ( dlx -- )
  dup .row_count 1+ over .col_count 1+ alloc_matrix ( -- dlx mat )
  swap .root
  begin ( -- mat cel )
    all_cells_next
    dup node_is_root? invert 
    while
      2dup .row_idx mark_row_active
      2dup .col_idx mark_col_active
      2dup set_cell
  repeat
  drop ;
  
: col_marked_active? ( mat n -- )
  over .mat_row_count 1- swap mat_get 0<> ;
  
: row_marked_active? ( mat n -- )
  0 mat_get 0<> ;
  
: process_row { oarr mat r -- oarr }
  oarr mat .mat_col_count ( -- oarr n )
  1 ?do ( -- oarr )
    mat i col_marked_active? if
      mat r i mat_get
      over c! char+
    endif
  loop ;
  
: compress_matrix_resize { rs pos mat -- mat }
  mat rs 0<> if ( -- mat )
    rs pos mat - over / ( -- mat rs cs )
  else 0 0 endif
  resize_matrix ;
  
: compress_matrix { mat -- mat }
  0 mat mat .mat_row_count 1- 0 
  ?do ( -- rs marr )
    mat i row_marked_active? if
      mat i process_row  ( -- rs marr )
      swap 1+ swap
    endif
  loop 
  mat compress_matrix_resize ;
  
: dlx_print_matrix ( dlx -- )
  mark_active_cells 
  compress_matrix
  dup print_matrix
  free_matrix ;
  
." included dlx_utils.fs" cr
