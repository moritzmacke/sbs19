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
  dup dup .up .down_field ! ( -- n )
  dup .down .up_field ! ( -- )
  ;
  
: restore_left_right ( n -- )
  dup dup .left .right_field ! ( -- n )
  dup .right .left_field ! ( -- )
  ;
  
\ Node Operations
  
: link_self ( addr -- )
  dup dup .left_field ! ( addr -- addr )
  dup dup .right_field ! 
  dup dup .up_field ! 
  dup .down_field ! ( addr --  )
  ;
  
\ now with macros...  
  
: insert_upxx ( n1 n2 -- )
  2dup .down_field !                       \ n2.down = n1
  swap .up_field 2dup @ swap .up_field !   \ n2.up = n1.up  ( -- n2 n1.up )
  2dup @ .down_field ! !                   \ n1.up.down = n2; n1.up = n2
  ;
  
: [insert] { xt1 xt2 } \ runtime ( n1 n2 -- )
  postpone 2dup xt2 compile, postpone ! 
  postpone swap xt1 compile, postpone 2dup postpone @ postpone swap xt1 compile, postpone !
  postpone 2dup postpone @ xt2 compile, postpone ! postpone !
  ; immediate
  
\ insert n2 to left of n1
: insert_left ( n1 n2 -- ) 
  [ ' .left_field ' .right_field ] [insert] ;
  
\ insert n2 to right of n1
: insert_right ( n1 n2 -- ) 
  [ ' .right_field ' .left_field ] [insert] ;
  
\ insert n2 above n1
: insert_up ( n1 n2 -- ) 
  [ ' .up_field ' .down_field ] [insert] ;
  
\ insert n2 below n1
: insert_down ( n1 n2 -- ) 
  [ ' .down_field ' .up_field ] [insert] ;
  
  
\ Row-Cell Operations
  
: block_node ( addr -- )
  dup unlink_up_down ( -- n)
  .column .length_field decrement
  ;
  
: unblock_node ( addr -- )
  dup restore_up_down
  .column .length_field increment
  ;
  
\ all very similar, traverse linked list and apply function to all but self
\ trying with macro

: [apply_exclusive] { nxt xt -- } \ runtime: ( addr -- )
  postpone dup nxt compile,
  postpone begin postpone 2dup postpone <> postpone while
  postpone dup xt compile, nxt compile,
  postpone repeat postpone 2drop
  ; immediate
  
\ Row Operations
  
: block_row ( addr -- )
  [ ' .right ' block_node ] [apply_exclusive] ;
  
: unblock_row ( addr -- )
  [ ' .left ' unblock_node ] [apply_exclusive] ;
  
\ Cover, uncover
  
: cover ( addr -- )
  dup unlink_left_right
  [ ' .down ' block_row ] [apply_exclusive] ;
  
: uncover ( addr -- )
  dup [ ' .up ' unblock_row ] [apply_exclusive]
  restore_left_right ;
  
: .column+cover .column cover ; 

: .column+uncover .column uncover ;
  
: cover_all ( addr -- )
  [ ' .right ' .column+cover ] [apply_exclusive] ;
  
: uncover_all ( addr --)
  [ ' .left ' .column+uncover ] [apply_exclusive] ;
  
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
  swap cells dump true ;

: collect_solution ( [x0 ... xn] skip top -- [x0 ... xn] skip addr len )
  sp@ 2 cells + tuck - ( -- ... xn skip src sz )
  dup 1 cells / -rot ( -- ... xn skip len src sz )
  dup mem_alloc ( --- ... len src sz dst )
  swap 0 ?do ( --- ... len src dst )
    over i + @ .row_idx 
    over i + !
  cell +loop
  nip swap ;
   
: row_is_blocked ( node -- flag )
  dup .up .down over <> ( -- node flag)
  swap .column dup .left .right <> ( -- flag flag)
  or ;
  
: set_choice ( dlx rid -- n )
  dlx_get_row dup row_is_blocked if
    s" partial solution is invalid" exception throw
  endif
  dup .column cover
  dup cover_all ;
  
: set_choices { dlx addr len -- [c0 ... cn] }
  len 0 ?do
    dlx addr i cells + @ set_choice
  loop ;
  
: unset_choices ( [0 .. cx] -- 0 )
  begin
    dup 0<> while ( -- ci )
    dup uncover_all
    .column uncover
  repeat ;
  
: set_partial_solution ( dlx -- [c0 ... cn] )
  dup .partial_solution_length 
  dup 0<> if ( -- dlx len )
    over .partial_solution swap ( -- dlx addr len )
    set_choices
  else 2drop endif ; 
  
: columns_empty? ( dlx -- flag )
  .root dup .right = ;
  
: next_column ( [ c0 ... cn ] cx dlx  -- [ c0 ... cn cx ] col cy )
  best_column dup cover ( -- ... cx col ) 
  dup .down ;           ( -- ... cx col cy ) 
  
: set_done_true ( -- flag )
  true postpone literal ; immediate
  
: set_done_false ( -- flag )
  false postpone literal ; immediate
  
: handle_recovery ( [c0 .. cn] col flag -- ... flag )
  if cr ." continuing search" cr                        \ find next solution?
    over uncover_all          ( -- ... cn col )         \ undo choice
    swap .down set_done_false ( -- ... col cn' flag )   \ proceed to next on same column
  else cr ." aborting search" cr
    drop unset_choices drop set_done_true ( -- flag)
  endif ;
  
: handle_backtrack ( ... cn last -- ...? flag )
  over <> if                                            \ not reached top?
    dup .column               ( -- ... cn col )
    over uncover_all          ( -- ... cn col )
    swap .down set_done_false ( -- ... col cn' flag )   \ proceed to next on previous level
  else ( -- [0 ... cn] )
    unset_choices drop set_done_true ( -- flag )
  endif ;
    
: dlx_solve { dlx report_solution_xt -- }
  0 sp@ { top } ( -- guard )
  dlx set_partial_solution ( -- [c0 .. cn] )
  dup { last }                                  \ so we don't search past provided solution and undo it
  dlx next_column   
  begin ( -- ... cn col cx )
\    dlx dlx_print_matrix
    2dup <> if                                  \ see if back at column head
      dup cover_all                             \ otherwise eliminate current row
      dlx columns_empty? if                     \ if matrix empty we have solution
        swap top collect_solution ( -- ... col arr len )
        report_solution_xt execute ( -- ... col flag )
        handle_recovery ( -- ... flag)
      else
        nip dlx next_column set_done_false
      endif
    else                                        \ undo column choice 
      drop uncover ( -- ... cn )
      last handle_backtrack ( -- ...? flag)
    endif
  until
  ." done" cr
  ;

." included dlx_base.fs" cr
