\ ------------------------------- Cell & Column ------------------------------- 

struct
  cell% field .row_idx_field
  cell% field .col_idx_field
  cell% field .column_field
  cell% field .left_field
  cell% field .right_field
  cell% field .up_field
  cell% field .down_field
end-struct node%

node%
  cell% field .length_field
end-struct column%
 
: .row_idx .row_idx_field @ ;
: .column .column_field @ ;
: .left .left_field @ ;
: .right .right_field @ ;
: .up .up_field @ ;
: .down .down_field @ ;

\ column
: .col_idx .col_idx_field @ ;
: .length .length_field @ ;

\ container

struct 
  cell% field .row_count_field
  cell% field .col_count_field
  cell% field .col_array_capacity_field
  cell% field .col_array_field \ array of pointers
  cell% field .row_array_capacity_field
  cell% field .row_array_field
  column% field .root
  cell% field .partial_solution_field
  cell% field .partial_solution_length_field
end-struct dlx%

: .row_count ( addr -- n )
  .row_count_field @ ;
  
: .col_count ( addr -- n )
  .col_count_field @ ;
  
: .col_array_capacity .col_array_capacity_field @ ;
: .row_array_capacity .row_array_capacity_field @ ;
: .col_array .col_array_field @ ;
: .row_array .row_array_field @ ;

: dlx_set_column ( dlx n addr -- )
  -rot cells swap .col_array + ! ( -- addr arr)
  ;
  
: dlx_set_row ( dlx n addr -- )
  -rot cells swap .row_array + !
  ;
  
: dlx_get_column ( dlx n -- addr )
  cells swap .col_array + @
  ;
  
: dlx_get_row ( dlx n -- addr )
  cells swap .row_array + @
  ;
  
: .partial_solution .partial_solution_field @ ;
: .partial_solution_length .partial_solution_length_field @ ;
  
." included dlx_defs.fs" cr
