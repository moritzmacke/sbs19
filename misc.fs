
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
  ." calls to free: " mem_stat_frees @ . cr ;
  
: mem_alloc ( n -- addr )
  allocate 0<> if s" memory error" throw endif
  mem_stat_allocs increment ;
  
: %mem_alloc ( n n -- addr )
  %alloc mem_stat_allocs increment ;
  
: mem_resize ( addr n -- addr )
  dup 1 cells > if
    resize 0<> if s" memory error" throw endif
  else drop endif ;
   
: mem_free ( addr -- )
  free 0<> if s" memory error" throw endif
  mem_stat_frees increment ;
  
: alloc_empty ( n -- addr ) 
  dup mem_alloc
  dup rot 0 fill ( n addr -- addr )
  ;

: allot_empty ( n -- addr )
  here over allot ( -- n addr )
  tuck swap 0 fill ( -- addr addr n)
  ;
  
: unallot_above ( addr -- )
  here - allot ;
  
\ addr, old, req
: dyn_resize ( addr n1 n2 -- addr n1/n3 )
    2dup < if ( -- addr old req)
      nip 15 * 10 cells / 1+ cells ( -- addr new )
      tuck mem_resize swap
    else drop endif ;
  
\ matrix
 
: [matrix_size]  \ runtime ( rs cs -- mat )
  postpone * postpone chars 
  2 cells postpone literal postpone + ; immediate
  
: [matrix_init] \ runtime ( rs cs addr -- mat )
  postpone tuck postpone 2swap postpone ! postpone cell+ 
  postpone tuck postpone ! postpone cell+ ; immediate
  
: allot_matrix
  2dup [matrix_size] allot_empty [matrix_init] ;
  
: alloc_matrix
  2dup [matrix_size] alloc_empty [matrix_init] ;
  
: resize_matrix { mat rs cs -- mat }
  rs cs mat 2 cells - rs cs [matrix_size] mem_resize [matrix_init] ;
  
: allot_matrixx ( rs cs -- mat )
  2dup * chars 2 cells + allot_empty ( -- rs cs addr )
  tuck 2swap ! cell+ tuck ! cell+
  ;
      
: free_matrix ( addr -- )
  2 cells - mem_free ;

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
  
: [arr_for_all] { stp xt@ xt } ( runtime: addr n -- )
  stp postpone literal postpone * postpone over postpone + postpone swap ( -- end addr)
  postpone begin
    postpone 2dup postpone <> postpone while
    postpone dup xt@ compile, xt compile, 
    stp postpone literal postpone +
  postpone repeat
  postpone 2drop
  ; immediate
  
: [@_arr_for_all] { xt } ( runtime: addr n )
  1 cells ['] @ xt postpone [arr_for_all]
  ; immediate
  
: [c@_arr_for_all] { xt } ( runtime: addr n )
  1 chars ['] c@ xt postpone [arr_for_all]
  ; immediate
  
: arr_for_all ( addr n xt -- )
  swap 0 ?do ( -- addr xt )
    over i cells + @ over execute
  loop
  2drop
  ;

\ ----------------------------------- Print -----------------------------------
  
: print_arr ( addr n -- )
  [ ' . ] [@_arr_for_all] cr ;
  
: print_ch_arr ( addr n -- )
  [ ' . ] [c@_arr_for_all] cr ;
  
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
