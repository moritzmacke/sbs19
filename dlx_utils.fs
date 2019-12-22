require dlx_base.fs 
require myiter.fs 


\ ------------------------------- Node Iterator -------------------------------
  
  
: col_arr_iter ( n addr -- iter )
  column% %size swap arr_ptr_iter
  ;
  
iter%
  cell% field .node_iter_next
  cell% field .node_iter_end
end-struct node_iter%

: node_iter_has_next_xt ( iter -- )
    .node_iter_next @ 0<>
    ;
  
: node_lr_iter_next_xt { iter -- }
  iter .node_iter_next dup @ tuck ( -- nxt *nxt nxt )
  0= if ( -- nxt *nxt)
    s" No more items." exception throw 
  endif
  over iter .node_iter_end @  ( -- nxt *nxt nxt end )
    = if ( -- nxt *nxt ) \ this was last node
    0 swap ! ( -- nxt )
  else
    over .right swap ! ( -- nxt )
  endif
  ; 

\ from to inclusive
: node_lr_iter { s e -- iter }
  node_iter% iter_create
  s over .node_iter_next ! ( iter -- iter )
  e over .node_iter_end !
  ['] node_iter_has_next_xt over .iter_has_next_xt !
  ['] node_lr_iter_next_xt over .iter_next_xt !
  ;
  
  
\ no iter for now...
\ from to exclusive
\ not worth it?

: for_all_right ( s e xt  -- )
  -rot swap 
  begin 
    2dup <> while ( -- xt e s )
    rot 2dup execute ( -- e s xt )
    -rot .right ( -- xt e s.r)
  repeat
  2drop drop
  ;
  
: for_all_left
  ;
  
: for_all_up
  ;
  
: for_all_down
  ;
  
  
: bin_arr_to_positions ( arr n -- arr n )

  swap

  over cells mem_alloc 
  0 2swap swap ( n src dst --  dst 0 src n )
  0 ?do ( -- dst j src )
    i chars over + c@ ( -- dst j src c )
    0<> if ( -- dst j src )
      -rot 2dup cells + ( -- src dst j off )
      i swap ! ( -- src dst j )
      1+ rot ( -- dst j src )
    endif
  loop
  drop tuck ( -- j dst j )
  cells mem_resize 
  
  swap
  ;

  
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
    else
      exit
    endif
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

    
: print_active_cols ( dlx -- )
  .root dup .right swap .left node_lr_iter ['] print_column for_all ; 
    
: print_cols ( dlx -- )
  dup .col_array swap .col_count ['] print_column arr_for_all
  ;
  
  
: print_active_nodes ( dlx -- )
  .root 
  begin
    all_cells_next
    dup node_is_root? false =
    while dup print_node
  repeat
  drop
  ;
    
\ ----------- printing the sparse matrix -------------

: mark_row_active ( mat row -- )
  3 -rot 0 mat_set
  ;
  
: mark_col_active ( mat col -- )
  1+ 3 -rot over .mat_row_count 1- swap mat_set
  ;

: set_cell ( mat cel -- )
  1 -rot dup .row_idx swap .col_idx 1+ mat_set 
  ;
  
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
  drop
  ;
  
: col_marked_active? ( mat n -- )
  over .mat_row_count 1- swap mat_get 0<>
  ;
  
: row_marked_active? ( mat n -- )
  0 mat_get 0<> ;
  
: process_row { oarr mat r }
  oarr mat .mat_col_count ( -- oarr n )
  1 ?do ( -- oarr )
    mat i col_marked_active? if
      mat r i mat_get
      over c! char+
    endif
  loop
  ;
  
: compress_matrix { mat -- mat }
  0 mat ( -- rs mat )
  mat .mat_row_count 1- 0 
  ?do ( -- rs marr )
    mat i row_marked_active? if
      mat i process_row  ( -- rs marr )
      swap 1+ swap
    endif
  loop 
  over 0 = if
    2drop 0 0
  else
    mat - over / ( -- rs cs )
  endif
  
  mat -rot
  resize_matrix
;
  
  
: dlx_print_matrix ( dlx -- )
  mark_active_cells 
  compress_matrix
  dup print_matrix
  free_matrix
  ;
  
." included dlx_utils.fs" cr
