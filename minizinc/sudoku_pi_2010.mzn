% 
% Pi Day Sudoku 2010 in MiniZinc.
% 
% http://brainfreezepuzzles.com/main/piday2010.html
% """
% This puzzle only has 18 clues! That is conjectured to be 
% the least number of clues that a unique-solution rotationally 
% symmetric puzzle can have.
%
% To celebrate Pi Day, the given clues are the first 18 digits 
% of π = 3.14159265358979323...
% """
%
% Via
% http://threesixty360.wordpress.com/2010/03/09/pi-day-sudoku-is-back/
% 

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%
include "globals.mzn";
int: S;
int: n = S * S;

array[1..n,1..n] of 0..9: hints = array2d(1..n,1..n,
          [7,2,0, 0,0,0, 0,0,0,
           0,5,0, 0,0,9, 0,0,0,
           0,0,0, 0,3,8, 0,0,0,

           0,0,0, 4,0,0, 5,0,0,
           0,0,3, 0,0,0, 9,0,0,
           0,0,1, 0,0,3, 0,0,0,

           0,0,0, 2,5,0, 0,0,0,
           0,0,0, 6,0,0, 0,3,0,
           0,0,0, 0,0,0, 0,1,9]);

array[1..n,1..n] of var 1..n: x;

% solve satisfy;
solve :: int_search(
        [x[i,j] | i,j in 1..n], 
        first_fail, 
        indomain_min, 
        complete) 
    satisfy;
% solve :: labelling_ff satisfy;

constraint 
      forall(i,j in 1..n) (
        if hints[i,j] > 0 then
            x[i,j] = hints[i,j]
        else 
            true
        endif
      )
;

% using :: domain means that Gecode/fz and JaCoP/FlatZinc
% solves this with 0 failures. That's nice.
constraint 
  forall(i in 1..n) (
      all_different([x[i,j] | j in 1..n]) :: domain
  )
;

constraint
  forall(j in 1..n) (
    all_different([x[i,j] | i in 1..n]) :: domain
  ) 
;

constraint
  forall(i in 0..2,j in 0..2) (
    all_different([x[r,c] | r in i*3+1..i*3+3, c in j*3+1..j*3+3] ) :: domain
  )
;


% alternative version
%       /\
%       forall (i, j in XRange, k in 1..(j - 1)) (
% 	x[i,j] != x[i,k] /\
%         x[j,i] != x[k,i]
%       )
%       /\
% 	forall (a, o, a1, o1, a2, o2 in SubSquareRange
% 		where a1 != a2 /\ o1 != o2) (
% 		x[(a - 1) * S + a1, (o - 1) * S + o1] !=
% 			x[(a - 1) * S + a2, (o - 1) * S + o2]
% 	)



S=3;

output [
  if j = 1 then "\n" else " " endif ++
  show(x[i,j])
  | i,j in 1..n
] ++ ["\n"];
