require misc.fs

\ ---------------------------------- Iterator --------------------------------- 

struct
  cell% field .iter_has_next_xt
  cell% field .iter_next_xt
  cell% field .iter_free_xt
end-struct iter%

: iter_next? ( addr -- addr )
  dup .iter_has_next_xt @ execute ;
  
: iter_next ( addr -- addr x ) \ dont consume iter?
  dup .iter_next_xt @ execute ;

iter%
  cell% field .arr_ptr_iter_step
  cell% field .arr_ptr_iter_next
  cell% field .arr_ptr_iter_end
end-struct arr_ptr_iter%

variable iter_created_count
variable iter_freed_count

: iter_internal_free ( addr -- )
  mem_free 
  iter_freed_count decrement
  ;

: iter_create ( n n -- addr )
  %alloc
  dup .iter_free_xt ['] iter_internal_free swap !
  iter_created_count increment
  ;
 
: iter_free ( addr ) 
  .iter_free_xt @ execute ;
  
: for_all { iter xt -- }
  begin
    iter iter_next?
    while
    iter iter_next
    xt execute
  repeat
  ;
  
\ Arrays
  
: arr_ptr_iter_has_next_xt ( iter -- )
  dup .arr_ptr_iter_next @ ( . -- iter n )
  swap .arr_ptr_iter_end @ <
  ;
  
: arr_ptr_iter_next_xt { iter -- }
  iter arr_ptr_iter_has_next_xt invert if 
    s" No more items." exception throw 
  endif
  iter .arr_ptr_iter_next dup @ tuck ( -- nxt *nxt nxt )
  iter .arr_ptr_iter_step @ + ( . -- nxt *nxt nxt_nxt )
  swap ! ; \ returns pointer to array item...
   
\ could do with creating xt at runtime?
: arr_ptr_iter { n s arr -- iter }
  arr_ptr_iter% iter_create
  s over .arr_ptr_iter_step ! ( iter -- iter )
  arr over .arr_ptr_iter_next !
  n s * arr + over .arr_ptr_iter_end !
  ['] arr_ptr_iter_has_next_xt over .iter_has_next_xt !
  ['] arr_ptr_iter_next_xt over .iter_next_xt !
  ;
  
: arr_cell_iter_next_xt
  arr_ptr_iter_next_xt @ ;
  
: arr_cell_iter ( n arr -- iter )
  cell swap 
  arr_ptr_iter
  ['] arr_cell_iter_next_xt
  over .iter_next_xt ! 
  ; 
  
: arr_ch_iter_next_xt
  arr_ptr_iter_next_xt 
  c@ 
  ;
  
: arr_ch_iter ( n arr -- iter )
  1 chars swap
  arr_ptr_iter
  ['] arr_ch_iter_next_xt
  over .iter_next_xt !
  ;
  

\ ---------------------------------- Counter --------------------------------- 

  

iter%
  cell% field .pc_counter_field
  cell% field .pc_sub_iter_field
end-struct numerator%

: .pc_sub_iter ( addr -- addr )
  .pc_sub_iter_field @ ;


: counter_has_next_xt ( iter -- )
  .pc_sub_iter iter_next? ;
  
\ returns 2 items
: counter_next_xt { iter -- }
  iter .pc_sub_iter iter_next
  iter .pc_counter_field dup @ ( -- nxt *cnt cnt )
  swap increment
  ;

\ would probably need to free sub iter also at free?
  
: numerator ( iter -- iter )
    numerator% iter_create ( -- sub iter )
    tuck .pc_sub_iter_field ! (  -- iter )
    0 over .pc_counter_field !
    ['] counter_next_xt over .iter_next_xt !
    ['] counter_has_next_xt over .iter_has_next_xt !
  ;
  
\ eh, too complicated ...
  
iter%
  cell% field .filter_xt_field
  cell% field .filter_sub_iter_field
end-struct filter%

: .filter_xt ( addr -- addr )
  .filter_xt_field @ ;

: filter_has_next_xt ( iter -- )
  .filter_sub_iter_field @ iter_next? ;
  
: filter_next_xt { iter -- }
\  has next..
  
\  iter .filter_sub_iter @ iter .filter_xt ( -- iter xt )
\  repeat
  
  
\    xt execute...
\  until
  
  ;

: filter ( iter xt -- iter )
  filter% iter_create
  tuck .filter_sub_iter_field !
  ['] filter_next_xt over .iter_next_xt !
  ['] filter_has_next_xt over .iter_has_next_xt !
;

." included iter.fs" cr
