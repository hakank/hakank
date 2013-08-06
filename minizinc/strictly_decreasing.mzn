% 
% Global constraints 
%   strictly_decreasing 
%   strictly_increasing 
%   decreasing 
% in MiniZinc.
% 

% 
% From Global Constraint Catalogue
% 

% strictly_decreasing:
% http://www.emn.fr/x-info/sdemasse/gccat/Cstrictly_decreasing.html
% """
% strictly_decreasing​(VARIABLES)
% 
% The variables of the collection VARIABLES are strictly decreasing.
% 
% Example
% (<8, 4, 3, 1>)
%
% The strictly_decreasing constraint holds since 8 > 4 > 3 > 1.
%
% """

% strictly_increasing:
% http://www.emn.fr/x-info/sdemasse/gccat/Cstrictly_increasing.html
% """
% strictly_increasing(VARIABLES)
% 
% The variables of the collection VARIABLES are strictly increasing.
% 
% Example
% (<1, 3, 4, 8>)
%
% The strictly_increasing constraint holds since 1 < 3 < 4 < 8.
%
% """

% decreasing:
% http://www.emn.fr/x-info/sdemasse/gccat/Cdecreasing.html
% """
% decreasing​(VARIABLES)
% 
% The variables of the collection VARIABLES are decreasing.
% 
% Example
% (<8, 4, 1, 1>)
% 
% The decreasing constraint holds since 8 >= 4 >= 1 >= 1.
% """



% 
% MiniZinc model created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc

% include "globals.mzn"; 

int: n = 4;
array[1..n] of var 1..8: x;

solve satisfy;


predicate strictly_decreasing(array[int] of var int: a) =
    forall(i in index_set(a) diff { min(index_set(a)) }) (a[i-1] > a[i])
;

predicate strictly_increasing(array[int] of var int: a) =
    forall(i in index_set(a) diff { min(index_set(a)) }) (a[i-1] < a[i])
;

predicate decreasing(array[int] of var int: a) =
    forall(i in index_set(a) diff { min(index_set(a)) }) (a[i-1] >= a[i])
;
predicate cp1d(array[int] of var int: x, array[int] of var int: y) =
  assert(index_set(x) = index_set(y),
           "cp1d: x and y have different sizes",
     forall(i in index_set(x)) ( x[i] = y[i] ))
; 


constraint

  cp1d(x, [8,4,3,1])
  /\
  strictly_decreasing(x)

  % x = [1,3,4,8]
  % /\
  % strictly_increasing(x)

  %x = [8,4,1,1]
  %/\ 
  %decreasing(x)
;


output
[
 show(x)
];