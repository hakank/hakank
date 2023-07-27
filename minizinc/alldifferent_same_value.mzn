% 
% Global constraint alldiffrent_same_value in MiniZinc.
% 
% From Global Constraint Catalogue
% https://sofdem.github.io/gccat/gccat/Calldifferent_same_value.html
% """
% All the values assigned to the variables of the collection VARIABLES1 
% are pairwise distinct. 
% NSAME is equal to number of constraints of the form 
% VARIABLES1[i].var=VARIABLES2[i].var(1i|VARIABLES1|)
% that hold.
% 
% Example
%     (
%     2, 
%     <7, 3, 1, 5>,
%     <1, 3, 1, 7>
%     )
% 
%  The alldifferent_same_value constraint holds since:
% * All the values 7, 3, 1 and 3 are distinct,
% * Among the four expressions 7=1, 3=3, 1=1 and 5=7 exactly 2 
%   conditions hold.
% """

% Model created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc

include "globals.mzn"; 

int: n = 4;
array[1..n] of var 1..7: x;
array[1..n] of var 1..7: y;
var 0..n: nsame;

%
% all_different_same_value
%
predicate all_different_same_value(var int: nsame, array[int] of var int: x, array[int] of var int: y) =
  all_different(x)
  /\
  sum(i in index_set(x)) (bool2int(x[i] = y[i])) = nsame
;

predicate cp1d(array[int] of int: x, array[int] of var int: y) =
  assert(index_set(x) = index_set(y),
           "cp1d: x and y have different sizes",
    forall(i in index_set(x)) (
      x[i] = y[i]
    )
  )
; 

solve satisfy;

constraint
  cp1d([7,3,1,5],x)
  /\
  cp1d([1,3,1,7], y)
  /\
  all_different_same_value(nsame, x,y)
  % /\
  % nsame = 2
;


output
[
  "x: " ++ show(x) ++ "\n" ++
  "y: " ++ show(y) ++ "\n" ++
  "nsame: " ++ show(nsame) ++ "\n"
];
