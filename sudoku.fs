require dlx.fs

9 constant sudo_rows
9 constant sudo_cols
9 constant sudo_nums

3 constant sudo_blocks_h
3 constant sudo_blocks_v
sudo_blocks_h sudo_blocks_v * constant sudo_blocks

sudo_rows sudo_cols * constant sudo_cel_constraints
sudo_rows sudo_nums * constant sudo_row_constraints
sudo_cols sudo_nums * constant sudo_col_constraints
sudo_blocks sudo_nums * constant sudo_blk_constraints

0 constant sudo_cel_con_off
sudo_cel_con_off sudo_cel_constraints + constant sudo_row_con_off
sudo_row_con_off sudo_row_constraints + constant sudo_col_con_off
sudo_col_con_off sudo_col_constraints + constant sudo_blk_con_off

sudo_cel_constraints sudo_row_constraints +
sudo_col_constraints + sudo_blk_constraints +
constant sudo_constraints

sudo_rows sudo_cols * sudo_nums *
constant sudo_choices 

: choice_number ( row col num -- n )
  rot sudo_rows * ( -- col num n)
  rot + sudo_cols * + ( -- n )
  ;
  
: row_of_choice ( n -- n )
  sudo_cols sudo_nums * /
  ;
  
: col_of_choice ( n -- n )
  dup sudo_cols sudo_nums * / 
  sudo_cols sudo_nums * *
  - sudo_nums /
  ;
  
: num_of_choice ( n -- n )
  sudo_nums mod
  ;

: cel_constraint ( row col num -- n )
  drop swap sudo_cols * +
  sudo_cel_con_off +
  ;
  
: row_constraint ( row col num -- n )
  nip swap sudo_nums * +
  sudo_row_con_off +
  ;
  
: col_constraint ( row col num -- n )
  swap sudo_nums * + nip
  sudo_col_con_off +
  ;
  
: blk_constraint ( row col num -- n )
  rot sudo_blocks_v / sudo_blocks_h * ( -- col num n )
  rot sudo_blocks_h / + ( -- num n )
  sudo_nums * + sudo_blk_con_off +
  ;

\ put on stack in descending order -> ascending in memory
: gen_mrow_for { r c n -- n n n n }
  r c n blk_constraint
  r c n col_constraint
  r c n row_constraint
  r c n cel_constraint
  ;
  
: print_arr ( addr n -- )
  0 ?do
    dup i cells + @ .
  loop
  drop cr
  ;
  
: test_gen_mrows ( -- )
  sudo_rows 0 ?do
    sudo_cols 0 ?do
      sudo_nums 0 ?do
        k j i gen_mrow_for
        sp@ 4 print_arr 
        2drop 2drop
      loop
    loop
  loop
  ;
   
: sudo_print_number ( c -- )
  dup 0<> if
    .
  else
    ." _ " drop 
  endif
  ;

: sudo_print_puzzle ( mat -- )
  cr dup .mat_row_count 0 ?do
    dup i mat_get_row_arr
    ['] sudo_print_number ch_arr_for_all cr
  loop
  drop
  ;
   
: sudo_load_mrows { dlx -- }
  sudo_rows 0 ?do
    sudo_cols 0 ?do
      sudo_nums 0 ?do
        k j i gen_mrow_for sp@ 4 ( -- [] addr len)
        dlx k j i choice_number 2swap
        dlx_add_row
        2drop 2drop
      loop
    loop
  loop
  ;
  
: construct_partial_solution { mat -- addr len }
  here >r
  sudo_rows 0 ?do
    sudo_cols 0 ?do ( -- )
      j i mat j i mat_get
      dup 0<> if ( -- row col num )
        1- choice_number ,
      else
        2drop drop
      endif
    loop
  loop
  r> here over - cell /
  ;
  
: sudo_set_partial_solution ( dlx mat -- )
  construct_partial_solution dlx_set_partial_solution
  ;
  
variable sudoku_dlx

: sudo_init_dlx ( -- )
  dlx_init dup sudo_load_mrows
  sudoku_dlx !
  ;
  
: sudo_process_solutionx ( addr len -- flag )
  print_arr 0
  ;
  
: sudo_process_solution ( addr len -- flag )
  here >r
  sudo_rows sudo_cols allot_matrix -rot 
  0 ?do ( -- mat addr )
    2dup i cells + @ ( -- mat addr mat n )
    dup num_of_choice 1+ -rot ( -- mat addr num mat n )
    dup row_of_choice swap col_of_choice mat_set
  loop
  drop sudo_print_puzzle
  r> unallot_above -1
  ;
  
: sudo_solve ( -- )
  sudoku_dlx @ ['] sudo_process_solution dlx_solve
  ;
  
sudo_rows sudo_cols
matrix[ 0 c, 0 c, 0 c,  4 c, 0 c, 0 c,  3 c, 2 c, 0 c,
        5 c, 0 c, 9 c,  0 c, 0 c, 2 c,  7 c, 8 c, 6 c,
        1 c, 0 c, 0 c,  7 c, 0 c, 0 c,  9 c, 0 c, 0 c,
        4 c, 0 c, 0 c,  2 c, 0 c, 9 c,  6 c, 0 c, 0 c,
        6 c, 0 c, 0 c,  3 c, 0 c, 1 c,  0 c, 0 c, 2 c,
        0 c, 0 c, 2 c,  5 c, 0 c, 8 c,  0 c, 0 c, 7 c,
        0 c, 0 c, 1 c,  0 c, 0 c, 7 c,  0 c, 0 c, 4 c,
        9 c, 5 c, 4 c,  6 c, 0 c, 0 c,  1 c, 0 c, 8 c,
        0 c, 3 c, 7 c,  0 c, 0 c, 4 c,  0 c, 0 c, 0 c,
]matrix mat_var current_sudoku
  
: sudo_set_puzzle ( [c0 .. c80 ] mat -- )
  { mat }
  sudo_rows 0 ?do
    sudo_cols 0 ?do ( -- [... cx]  )
      mat sudo_rows 1- j - sudo_cols 1- i -
      mat_set
    loop
  loop
  ;
  
: set_puzzle ( [c0 .. c80 ] mat -- )
  current_sudoku sudo_set_puzzle
  ;
  
: solve_puzzle ( -- ) 
  here current_sudoku sudo_print_puzzle
  sudoku_dlx @ current_sudoku sudo_set_partial_solution
  sudo_solve
  unallot_above
  ;

sudo_init_dlx
  
solve_puzzle
