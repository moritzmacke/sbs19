\ ------------------------------- Cell & Column ------------------------------- 

struct
  cell% field .row_idx_field
  cell% field .column_field
  cell% field .left_field
  cell% field .right_field
  cell% field .up_field
  cell% field .down_field
end-struct node%

node%
  cell% field .col_idx_field
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
  column% field .root
end-struct dlx%

: .row_count ( addr -- n )
  .row_count_field @ ;
  
: .col_count ( addr -- n )
  .col_count_field @ ;
