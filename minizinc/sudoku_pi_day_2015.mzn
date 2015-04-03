% 
% Sudoku Pi Day 2015 in MiniZinc.
% 
% http://brainfreezepuzzles.com/piday2015.html
% """
% Fill in the grid so that 1-9 appear exactly once in each row, column, and block. In addition, 
% each shaded rectangle must contain exactly the digits in 3.1415926. Note that this puzzle only has 
% 16 clues, which is insufficient for a regular sudoku puzzle to have a unique solution.
% """
% Via https://threesixty360.wordpress.com/2015/03/10/happy-pi-day-2015/

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@gmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc/
%

include "globals.mzn"; 

int: n;
int: reg = ceil(sqrt(int2float(n))); % size of the regions

% the 4 sequences
int: num_seqs;
array[int] of int: seqs;
                             % 1 2 3 4 5 6 7 8 9
array[1..9] of int: seq_gcc = [2,1,1,1,1,1,0,0,1];

% decision variables
array[1..n, 1..n] of var 1..n: x;


solve :: int_search([x[i,j] | i, j in 1..n], most_constrained, indomain_min, complete) satisfy;

%
% All different in rows, columns and blocks.
%
constraint 

  forall(i in 1..n) (
    alldifferent([x[i,j] | j in 1..n])
  )
  /\
  forall(j in 1..n) (
    alldifferent([x[i,j] | i in 1..n])
  )   
  /\
  forall(i in 0..reg-1,j in 0..reg-1) (
    alldifferent([x[r,c] | r in i*reg+1..i*reg+reg, c in j*reg+1..j*reg+reg])
  )

  /\
  % 31415926 in each sequence
  forall(s in 0..num_seqs-1) (
    global_cardinality([x[seqs[(s*2*8)+(2*i+1)],seqs[(s*2*8)+(2*i+2)]] | i in 0..7], 
                       [i | i in 1..9], 
                       seq_gcc)
  )

;

output 
[
  if j = 1 then "\n" else " " endif ++
    show(x[i,j])
  | i,j in 1..n
] ++ ["\n"];

%
% data 
%

n = 9;
x = array2d(1..n,1..n,
 [
   _,_,_, 9,_,4, _,_,_,
   _,3,_, _,_,_, _,9,_,
   _,_,_, _,_,_, _,_,_,

   1,_,_, _,6,_, _,_,2,
   _,_,_, 4,_,2, _,_,_,
   6,_,_, _,3,_, _,_,4,

   _,_,_, _,_,_, _,_,_,
   _,5,_, _,_,_, _,4,_,
   _,_,_, 7,_,5, _,_,_,

 ]);


num_seqs = 4;
seqs = 
[
  % seq 1
        2,2,  2,3, 2,4, 
  3,1,  3,2,  3,3, 3,4,
  4,1,


  % seq 2
                    1,9,
  2,6,    2,7, 2,8, 2,9,
  3,6,    3,7, 3,8,

  % seq 3
  5,3, 5,4,
  6,3, 6,4,
  7,3, 7,4,
  8,3, 8,4,

  % seq 4
  6,6,   6,7,
  7,6,   7,7,
         8,7, 8,8,
         9,7, 9,8,
];
       