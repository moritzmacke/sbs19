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
  
  
: bin_arr_to_positions ( n arr -- n arr )
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
  ;
      
\ --------- Collect cells from whole matrix ----------

: list_cells_iter_has_next_xt ( iter -- flag )
    .node_iter_next @ .row_idx -1 <> ;

: find_next_node ( cur -- nxt )
  begin
    .down dup .col_idx ( -- nxt cid )
    -1 <> ( -- nxt flag ) \ stop at root
    over .row_idx -1 = and \ or when row node found
    while ( -- nxt ) \ head
      .right
  repeat
  ;
  
: list_cells_iter_next_xt ( iter -- addr )
  dup list_cells_iter_has_next_xt invert if s" " exception throw endif
  dup .node_iter_next @ tuck ( -- cur iter cur )
  find_next_node ( -- cur iter nxt )
  swap .node_iter_next !
  ;

\ call on root to collect all active cells
: list_cells_iter ( root -- iter )
  node_iter% %alloc swap
  .right find_next_node ( -- iter nxt )
  over .node_iter_next ! ( -- iter )
  ['] list_cells_iter_next_xt over .iter_next_xt !
  ['] list_cells_iter_has_next_xt over .iter_has_next_xt !
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


: print_active_nodes ( dlx -- )
  .root list_cells_iter ['] print_node for_all ;
   
: print_active_nodes_full ( dlx -- )
  .root list_cells_iter ['] print_node_full for_all ;
  
: print_active_cols ( dlx -- )
  .root dup .right swap .left node_lr_iter ['] print_column for_all ; 
   
: print_cols ( dlx --  )  
  dup .root 
  swap .col_count 1+
  0 ?do
    dup print_column
    column% %size +
  loop
  drop ;
    
\ ----------- printing the sparse matrix -------------
              
: mark_active_colsx ( addr n -- n addr )
  chars allot_empty 0 -rot ( root cols  -- n root cs )
  begin
    swap .right tuck ( -- n cur cs cur )
    .col_idx ( -- n cur cs cid )
    dup 0 >= if
      chars over + ( -- n cur cs off )
      1 swap c! ( -- n cur cs )
      rot 1+ -rot ( -- n cur cs )
      0
    else \ how to do this loop right?
      drop -1
    endif
  until
  nip
  ;  
  
: mark_active_rowsx ( addr n -- n addr )
  chars allot_empty 0 -rot ( root rows -- n root rs )
  swap list_cells_iter swap ( -- n iter rs )
  begin
    over iter_next?
    while
    over iter_next .row_idx ( -- n iter rs rid )
    chars over + ( -- n iter rs off )  
    dup c@ 
    0= if ( -- n iter rs off )  
      1 swap c! ( -- n iter rs )
      rot 1+ -rot ( -- n iter rs )    
    else
      drop
    endif
  repeat
  nip
  ;
  
: mark_active_cells ( addr n n -- addr ) 
  tuck >r * alloc_empty ( root rows cols -- root mat ; R:cols )
  swap list_cells_iter swap ( -- iter mat ; R:cols)
  begin
    over iter_next?
    while
    over iter_next dup ( -- iter mat nxt nxt ; R:cols)
    .row_idx r@ * ( -- iter mat nxt roff ; R:cols)
    swap .col_idx + ( -- iter mat off ; R:cols)
    over + 1 swap c! ( -- iter mat ; R:cols)
  repeat
  nip rdrop
  ;

: compress_matrix { mat rs cs rows cols -- n mat }
  mat rows 0 ?do ( -- ci )
    rs i chars + c@ 1 = ( -- ci flag) \ active row
    if
    cols 0 ?do ( -- ci )
      cs i chars + c@ 1 = ( -- ci flag) \ i != i in outer loop
      if \ active column
        j cols * i + chars ( -- ci moff )
        mat + c@ ( -- ci src )
        over c! ( -- ci )
        1+
      endif
    loop
    endif
  loop
  mat - mat
  ;
    
: fill_matrix { root rows cols -- n n addr }
    here
    root rows cols mark_active_cells ( -- addr mat )
\    dup rows cols rot print_ch_mat cr
    root rows mark_active_rowsx rot ( -- addr nar rs mat )
\    over rows swap print_ch_arr
    root cols mark_active_colsx ( -- addr nar rs mat nac cs )
\    cols over print_ch_arr
    2swap -rot swap ( -- addr nar nac mat rs cs )    
    rows cols compress_matrix ( -- addr nar nac n mat )
    swap 2over ( -- addr nar nac mat n nar nac )
    * tuck <> if s" wrong size" exception throw endif ( -- ... mat sz )
    mem_resize 
    2swap swap ( addr nar nac mat -- nac mat nar addr )
    unallot_above -rot ( -- nar nac mat )
    ;
    
    
: dlx_print_matrix ( dlx -- )
  dup .root over .row_count ( -- dlx root rows)
  rot .col_count
  fill_matrix ( -- r c m )
  >r r@ print_ch_mat r> mem_free 
  ;
  
." included dlx_utils.fs" cr
