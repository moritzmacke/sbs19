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
  
: link_self ( addr -- )
  dup dup .left_field ! ( addr -- addr )
  dup dup .right_field ! 
  dup dup .up_field ! 
  dup .down_field ! ( addr --  )
  ;
  
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
  
: solution_callback_template ( len arr -- flag )
  swap cells dump -1
  ;

: collect_solution ( [x0 ... xi] skip top -- [x0 ... xi] skip src len )
  sp@ 2 cells + tuck - ( -- ... xi skip src sz )
  dup 1 cells / -rot ( -- ... xi skip len src sz )
  dup mem_alloc ( --- ... len src sz addr )
  swap 0 ?do ( --- .. len src addr )
    over i + @ .row_idx 
    over i + !
  cell +loop
  nip swap
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
  
: row_is_blocked ( node -- flag )
  dup .up .down over <> ( -- node flag)
  swap .column dup .left .right <> ( -- flag flag)
  or
  ;
  
: set_choice ( dlx rid -- n )
  dlx_get_row dup row_is_blocked if
    s" partial solution is invalid" exception throw
  endif
  dup .column cover
  dup cover_all
  ;
  
: set_choices { dlx addr len -- [c0 ... cn] }
  len 0 ?do
    dlx addr i cells + @ set_choice
\    dup ." set " print_node cr
  loop
  ;
  
: unset_choices ( [0 .. cx] -- 0 )
  begin
    dup 0<> while ( -- ci )
    dup uncover_all
    .column uncover
  repeat
  ;
  
: set_partial_solution ( dlx -- [c0 ... cn] )
  dup .partial_solution_length 
  dup 0<> if ( -- dlx len )
    over .partial_solution swap ( -- dlx addr len )
    set_choices
  else
    2drop
  endif
  ;
  
: dlx_solve { dlx report_solution_xt -- }
   
\  dlx dlx_print_matrix

  0 ( -- guard )
  sp@ { top }
 
  dlx set_partial_solution ( -- [c0 .. cn] )

  
  \ otherwise will continue to search past provided solution and undo it
  \ can surely be done better...
  dup { end }

  dlx best_column ( -- guard col ) 
  dup cover ( -- guard col ) \ remove column and all rows where == 1
  dup .down ( -- guard col n1 )
  
  begin ( -- ... n0 col n1 )
    2dup <> if \ not yet back at column head? -> haven't tried all rows yet
\      cr ." >>deeper" 
      dup cover_all ( -- ... n0 col n1 ) \ eliminate current row     
      dlx .root dup .right <> if \ test whether matrix not empty
        nip dlx best_column ( -- ... n0 n1 col ) \ no, then proceed to next column      
        dup cover ( -- ... n0 n1 col ) 
        dup .down ( -- ... n1 col n2 ) 
      else \ empty
        cr ." found solution" cr
        swap top collect_solution ( -- ... n0 n1 col arr len )
        report_solution_xt execute ( -- ... n0 n1 col flag )
        if \ search next solution?
          ." continuing search" cr
          over uncover_all ( -- ... n0 n1 col ) \ undo choice
          swap .down ( -- ... n0 col n1' ) \ proceed to next on same column
        else
          ." aborting search" cr
          drop unset_choices drop
          exit
        endif
      endif
    else ( -- ... n0 col n1 ) \ undo column choice
\      cr ." <<back-up" 
      drop uncover ( -- ... n* n0 )
      dup end <> if \ not reached top? 
        dup .column ( -- ... n* n0 col )
        over uncover_all ( -- ... n* n0 col )
        swap .down ( -- ... n* col n0') \ proceed to next on previous level
      else ( -- [0 ... cx] )
        ." done" cr
        unset_choices drop
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
