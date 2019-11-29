require misc.fs
require dlx_def.fs

require dlx_utils.fs

\ --------------------------------- Operations --------------------------------
  
\ Dancing Links Operations
  
: unlink_up_down { n -- } 
  n .down  n .up .down_field ! ( set-> )
  n .up    n .down .up_field ! ( set-> )
  ;
  
: unlink_left_right { n -- }
  n .right  n .left .right_field ! ( set-> )
  n .left   n .right .left_field ! ( set-> )
  ;
  
: restore_up_down ( n -- )
  dup ( -- n n )
  dup .up .down_field ! ( -- n )
  dup .down .up_field ! ( -- )
  ;
  
: restore_left_right ( n -- )
  dup ( -- n n )
  dup .left .right_field ! ( -- n )
  dup .right .left_field ! ( -- )
  ;
  
\ Node Operations
  
\ insert n2 to right of n1
: insert_right { n1 n2 -- }
  n1 n2 .left_field !
  n1 .right n2 .right_field !
  n2 n1 .right .left_field !
  n2 n1 .right_field ! ;
  
\ insert n2 to left of n1
: insert_left { n1 n2 -- }
  n1 n2 .right_field !
  n1 .left n2 .left_field !
  n2 n1 .left .right_field !
  n2 n1 .left_field ! ;
  
\ insert n2 above n1
: insert_up { n1 n2 -- }
  n1 n2 .down_field !
  n1 .up n2 .up_field !
  n2 n1 .up .down_field !
  n2 n1 .up_field ! ;
  
\ Row-Cell Operations
  
: block_node ( addr -- )
  dup unlink_up_down ( -- n)
  .column .length_field decrement
  ;
  
: unblock_node ( addr -- )
  dup restore_up_down
  .column .length_field increment
  ;
  
\ all very similar...
\ can this be done with macros somehow?
: macros?xxx
  ;
 
\ Row Operations
  
: block_row ( addr -- )
  dup .right
  begin
    2dup <> while ( -- n r)
    dup block_node
    .right
  repeat
  2drop
  ;
  
: unblock_row ( addr -- )
  dup .left
  begin
    2dup <> while
    dup unblock_node
    .left
  repeat
  2drop
  ;
   
\ Cols

: cover ( addr -- )
  dup unlink_left_right
  dup .down ( -- head cur )
  begin
    2dup <> while
    dup block_row
    .down
  repeat
  2drop
  ;
  
: uncover ( addr -- )
  dup .up ( -- head cur )
  begin
    2dup <> while
    dup unblock_row
    .up
  repeat
  drop
  restore_left_right
  ;
  
: cover_all ( addr -- )
    dup .right ( -- n1 nr )
    begin
\      ~~
      2dup <> while
      dup .column cover
      .right
    repeat
    2drop
  ;
  
: uncover_all ( addr --)
  dup .left ( -- n1 nl )
  begin
    2dup <> while
    dup .column uncover
    .left
  repeat
  2drop
  ;
  
\ --------------------------------- Algorithm ---------------------------------

variable dlx_stats_cols_searched

\ simple for now ...
: best_column ( dlx -- col )
  .root dup .right dup rot
  begin ( -- mc col root )
    dlx_stats_cols_searched increment
    2dup <> while
    -rot over .length ( -- root mc col ml ) 
    over .length > if
      nip dup ( -- root col col ) 
    endif
    .right rot
  repeat
  2drop
  ;
  
: report_solution ( len arr -- )
  swap cells dump
\  2drop 
  ;

: collect_solution ( ... xi skip top -- ... xi skip len src )
  sp@ 2 cells + tuck - ( -- ... xi skip src sz )
  dup 1 cells / -rot ( -- ... xi skip len src sz )
  dup mem_alloc ( --- ... len src sz addr )
  swap 0 ?do ( --- .. len src addr )
    over i + @ .row_idx 
    over i + !
  cell +loop
  nip
  ;
  
: report ( col addr -- col addr )
  cr
  ." column is " over .col_idx .
  ." node is " dup .row_idx . dup .col_idx . 
  ;
  
: print_path ( [n0 ... xi] col n -- [n0 ... xi] col n )
  sp@ 2 cells +
  begin
    dup @ 0<> while
    dup @ dup ." (" .row_idx . .col_idx . ." )"
    cell+
  repeat
  drop
;
  
: dlx_solve { dlx report_solution_xt -- }
   
\  dlx dlx_print_matrix
   
  0 ( -- guard )
  sp@ { top }
  dlx best_column ( -- guard col ) 
  dup cover ( -- guard col ) \ remove column and all rows where == 1
  dup .down ( -- guard col n1 )
  
  begin ( -- ... n0 col n1 )
    
\    report
\    dlx dlx_print_matrix

\   print_path
  
    2dup <> if \ not yet back at column head? -> haven't tried all rows yet
\      cr ." >>deeper" 
      dup cover_all ( -- ... n0 col n1 ) \ eliminate current row
\      dlx dlx_print_matrix
      
      dlx .root dup .right <> if \ test whether matrix not empty
      
\        dlx print_active_cols
      
        nip dlx best_column ( -- ... n0 n1 col ) \ no, then proceed to next column
        
\        dup print_node_full
        
        dup cover ( -- ... n0 n1 col ) 
        dup .down ( -- ... n1 col n2 ) 
      else \ empty
        cr ." found solution" cr
        swap top collect_solution ( -- ... n0 n1 col len arr )
        report_solution_xt execute ( -- ... n0 n1 col )
        over uncover_all ( -- ... n0 n1 col ) \ undo choice
        swap .down ( -- ... n0 col n1' ) \ proceed to next on same column
      endif
    else ( -- ... n0 col n1 ) \ undo column choice
\      cr ." <<back-up" 
      drop uncover ( -- ... n* n0 )
      dup 0<> if \ not reached guard? 
        dup .column ( -- ... n* n0 col )
        over uncover_all ( -- ... n* n0 col )
        swap .down ( -- ... n* col n0') \ proceed to next on previous level
      else ( -- guard )
        drop
\        dlx dlx_print_matrix
        exit \ this exits whole definition, don't know how to break loop otherwise
      endif
    endif
  again
  s" shouldn't be here!" exception throw
  ;
  
  
: dlx_solve_clean_code { dlx report_solution_xt -- }
  0 sp@ { top }
  dlx best_column 
  dup cover dup .down 
  begin 
    2dup <> if 
      dup cover_all 
      dlx .root dup .right <> if 
        nip dlx best_column         
        dup cover dup .down 
      else 
        swap top collect_solution 
        report_solution_xt execute
        over uncover_all swap .down 
      endif
    else 
      drop uncover 
      dup 0<> if 
        dup .column 
        over uncover_all swap .down
      else
        drop exit
      endif
    endif
  again
  s" shouldn't be here!" exception throw ;

." included dlx_base.fs" cr
