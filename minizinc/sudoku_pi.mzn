% 
% Pi Day Sudoku (2009) problem in MiniZinc.
%
% "Pi Day 2009"
%   http://brainfreezepuzzles.com/main/piday2009.html
%
%   Via 360 "Pi Day Sudoku 2009"
%   http://threesixty360.wordpress.com/2009/03/09/pi-day-sudoku-2009/
%   """
%   Each row, column, and region contains the digits 1-9 exactly once plus 
%   three π symbols.  There’s a printable .pdf file here [http://brainfreezepuzzles.com/main/files/Brainfreeze_PiDay2009.pdf]
%
%   As a bonus, if you send a correct solution in to Brainfreeze puzzles in 
%   the next couple months, you're eligible for a drawing for their book 
%   on Color Sudoku!  More details are on their website [[http://brainfreezepuzzles.com/]].
%   """

% Note: To present is somewhat nicer, mzn_show.pl may be used like this:
%   ... | mzn_show.pl "{tr:-1:P:}"
%
% which replaces "-1" to "P".
%
% mzn_show.pl is here: http://www.hakank.org/minizinc/mzn_show.pl
%

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

include "globals.mzn"; % standard

int: n = 12;
int: X = 0;  % the unknown
int: P = -1; % Pi

% Using :: domain with Gecode/fz and JaCoP/FlatZinc gives
% 1 failure. With :: bounds is 53 failires, 
% and without any annotation: 31316 failures
predicate check(array[int] of var int: x) =
   global_cardinality(x, array1d(-1..9, [i | i in -1..9]), occ) :: domain % :: bounds
;

array[1..n, 1..n] of var -1..9: x;
array[-1..9] of 0..3: occ = array1d(-1..9, [3, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1]);
array[1..11] of 0..3: occ2 = [3, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1];

% solve satisfy;
solve :: int_search(
        [x[i,j] | i,j in 1..n], 
        max_regret, 
        indomain_max, 
        complete) 
    satisfy;

constraint

  % copy the hints
  forall(i in 1..n, j in 1..n) (
      x[i,j] != 0
      /\
      if dat[i,j] != X then
        x[i,j] = dat[i,j]
      else 
        true
      endif
  )

  /\ % rows
  forall(i in 1..n) (
    check([x[i,j] | j in 1..n]) 
  )

  /\ % columns
  forall(j in 1..n) (
    check([x[i,j] | i in 1..n])
  )

  /\ % the regions
  forall(i in 1..num_regions) (
    check([x[regions[j,1],regions[j,2]] | j in 1+(i-1)*n..1+((i-1)*n)+11 ])
  )

;


% output 
% [ show(occ) ++ "\n"] ++
% [
%   if j = 1 then "\n" else " " endif ++
%    show(x[i,j])
%   | i in 1..n, j in 1..n

% ] ++ ["\n"];


%
% data
%
array[1..n,1..n] of int: dat = array2d(1..n, 1..n,
[
 4,9,7, P,5,X,X,X,X, X,X,X,
 X,P,X, 8,X,X,9,6,1, 5,2,X,
 X,8,X, 1,X,X,X,P,X, 7,X,X,
 X,X,X, X,X,X,X,P,X, 4,X,X,
 5,3,9, 6,X,X,X,X,X, X,X,X,

 9,4,X, P,P,P,7,X,X, X,X,X,
 X,X,X, X,X,6,2,5,P, X,7,4,
 X,X,X, X,X,X,X,X,P, P,3,8,
 X,7,8, 4,6,9,X,X,X, X,X,X,

 X,X,3, X,P,X,X,4,7, 1,6,9,
 X,X,4, X,1,X,X,X,6, X,P,X,
 X,X,X, X,X,X,X,X,4, X,5,X
 ]);


%
% The regions
%
int: num_regions = 12;
array[1..num_regions*12, 1..2] of int: regions  = array2d(1..num_regions*12, 1..2,
[
 % Upper left dark green
  1,1  , 1,2  , 1,3  , 
  2,1  , 2,2  , 2,3  , 
  3,1  , 3,2  , 
  4,1  , 4,2  ,  
  5,1  , 5,2  , 
 
 % Upper mid light dark green
  1,4  ,  1,5  ,  1,6  ,  1,7  ,  1,8  ,  1,9  , 
  2,4  ,  2,5  ,  2,6  ,  2,7  ,  2,8  ,  2,9  , 

 % Upper right green
  1,10  ,  1,11  ,  1,12  , 
  2,10  ,  2,11  ,  2,12  , 
  3,11  ,  3,12  , 
  4,11  ,  4,12  , 
  5,11  ,  5,12   , 

 % Mid upper left "blue"
  3,3  ,  3,4  , 3,5  ,  3,6  , 
  4,3  ,  4,4  , 4,5  ,  4,6  , 
  5,3  ,  5,4  , 5,5  ,  5,6  , 

 % Mid Upper right blue
  3,7  ,  3,8  ,  3,9  ,  3,10  , 
  4,7  ,  4,8  ,  4,9  ,  4,10  , 
  5,7  ,  5,8  ,  5,9  ,  5,10  , 

 % Mid left green
  6,1  ,  6,2  , 6,3  , 
  7,1  ,  7,2  , 7,3  , 
  8,1  ,  8,2  , 8,3  , 
  9,1  ,  9,2  , 9,3  , 

 % Mid left blue
  6,4  , 6,5  , 
  7,4  , 7,5  , 
  8,4  , 8,5  , 
  9,4  , 9,5  , 
  10,4 , 10,5  , 
  11,4 , 11,5  , 

 % Mid mid green
  6,6  , 6,7  , 
  7,6  , 7,7  , 
  8,6  , 8,7  , 
  9,6  , 9,7  , 
  10,6 , 10,7  , 
  11,6 , 11,7  , 

 % Mid right blue
  6,8  ,  6,9  , 
  7,8  ,  7,9  , 
  8,8  ,  8,9  , 
  9,8  ,  9,9  , 
  10,8 ,  10,9  , 
  11,8 ,  11,9  , 

 % Mid right green
  6,10  ,  6,11  ,  6,12  , 
  7,10  ,  7,11  ,  7,12  , 
  8,10  ,  8,11  ,  8,12  , 
  9,10  ,  9,11  ,  9,12  , 

 % Lower left dark green
  10,1  , 10,2  ,  10,3  , 
  11,1  , 11,2  ,  11,3  , 
  12,1  , 12,2  ,  12,3  , 12,4  , 12,5  ,  12,6  , 

 % Lower right dark green
  10,10  ,  10,11  , 10,12  , 
  11,10  ,  11,11  , 11,12  , 
  12,7   ,  12,8   ,  12,9  , 12,10  , 12,11  ,  12,12  

]);


output 
[
  if j = 1 then "\n" else " " endif ++
    if fix(x[i,j]) = P then
      "P" 
    else 
      show(x[i,j])
    endif
  | i, j in 1..n
];