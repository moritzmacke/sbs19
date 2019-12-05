
\ ---------------------------------- Helpers ----------------------------------

: increment ( addr -- )
  1 swap +! ;
  
: decrement ( addr -- )
  -1 swap +! ;

\ ---------------------------------- Memory -----------------------------------
  
variable mem_stat_allocs
variable mem_stat_frees
\ variable mem_stat_total_alloc
\ variable mem_stat_total_free
  
: mem_stats
  ." calls to alloc: " mem_stat_allocs @ . cr
  ." calls to free: " mem_stat_frees @ . cr
  ;
  
: mem_alloc ( n -- addr )
  allocate 0<> if s" memory error" throw endif
  mem_stat_allocs increment
  ;
  
: mem_resize ( addr n -- addr )
  dup 1 cells > if
    resize 0<> if s" memory error" throw endif
  else
    drop
  endif
  ;
   
: mem_free ( addr -- )
  free 0<> if s" memory error" throw endif
  mem_stat_frees increment
  ;
  
: alloc_empty ( n -- addr ) 
  dup mem_alloc
  dup rot 0 fill ( n addr -- addr )
  ;

: allot_empty ( n -- addr )
  here over allot ( -- n addr )
  tuck swap 0 fill ( -- addr addr n)
  ;
  
: unallot_above ( addr -- )
  here - allot
  ;
  
\ addr, old, req
: dyn_resize ( addr n1 n2 -- addr n1/n3 )
    2dup < if ( -- addr old req)
      nip 15 * 10 cells / 1+ cells ( -- addr new )
      tuck mem_resize swap
    else
      drop
    endif
  ;
  
\ matrix

  
\ create empty char matrix
: allot_matrix { rows cols }
  rows cols * chars 2 cells + allot_empty
  rows over ! cell+ cols over ! cell+
  ;

: .mat_col_count ( mat -- u )
  1 cells - @ ;
  
: .mat_row_count ( mat -- u )
  2 cells - @ ;
  
: .mat_size ( mat -- u )
  dup .mat_row_count swap .mat_col_count * ;
  
: mat_get ( mat row col -- c )
  -rot over .mat_col_count * ( -- col mat roff )
  rot + chars + c@ 
  ;
  
: mat_set ( c mat row col )
  -rot over .mat_col_count * ( -- col mat roff )
  rot + chars + c!
  ;

: mat_get_row ( mat row -- addr )
  over .mat_col_count * chars + ;
  
: mat_get_row_arr ( mat row -- addr len )
  over .mat_col_count tuck ( -- mat len row len )
  * chars rot + swap
  ;

:  matrix[ ( rows cols -- addr )
   swap , , here ;
   
: ]matrix ( addr -- )
  here over - ( -- mat sz )
  over .mat_size <> if
  s" size of data doesn't match dimensions" exception throw endif
  ;
  
\ indirect matrix...
: mat_var ( "name" -- )
    create ,
  does>
    @
  ;
  
\ -- array

: arr_for_all ( addr n xt -- )
  swap 0 ?do ( -- addr xt )
    over i cells + @ over execute
  loop
  2drop
  ;
  
: ch_arr_for_all ( addr n xt -- )
  swap 0 ?do ( -- addr xt )
    over i chars + c@ over execute
  loop
  2drop
  ;

\ ----------------------------------- Print -----------------------------------
  
: print_ch_arr ( addr n -- )
  ['] . ch_arr_for_all cr ;
  
\ 
: print_ch_mat ( n n addr -- )
  rot 0 ?do ( -- n addr )
    cr
    over 0 ?do ( -- n addr )
      dup c@ .
      char+
    loop
  loop
  2drop cr
;

: print_matrix ( mat -- )
  dup .mat_row_count over .mat_col_count
  rot print_ch_mat ;

." included misc.fs" cr
