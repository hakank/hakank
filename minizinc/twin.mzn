% 
% Decomposition of twin global constraint in MiniZinc.
% 
% From Global catalog:
% """
% Pairs of variables related by hiden element constraints sharing the 
% same table.
%
% twin(PAIRS)
% 
% Purpose
% Enforce the condition 
%   PAIRS[i].x = u /\ PAIRS[i].y = v (i in [1, |PAIRS|]) 
%   => 
%   forall j in [1, |PAIRS|] : PAIRS[j].x = u <=> PAIRS[j].y = v.
%
% Example
% : collection(x−dvar, y−dvar)
%
% x-1 y-8
% x−9 y−6
% x−1 y−8,
% x−5 y−0,
% x−6 y−7,
% x−9 y−6
%
% The twin constraint holds since 
%   1 is paired with 8, 
%   9 is paired with 6, 
%   5 is paired with 0, 
%   6 is paired with 7.
% """


% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc/
%

include "globals.mzn"; 

int: n = 6;
array[1..n, 1..2] of var 0..9: pairs;
        
predicate cp2d(array[int,int] of var int: x, array[int,int] of var int: y) =
  assert(index_set_1of2(x) = index_set_1of2(y) /\
         index_set_2of2(x) = index_set_2of2(y),
           "cp2d: x and y have different sizes",
     forall(i in index_set_1of2(x), j in index_set_2of2(x)) (
         y[i,j] = x[i,j]
    ) 
  )
;
 
predicate twin(array[int,int] of var int: pairs) =
  forall(i in index_set_1of2(pairs)) (
     forall(j in index_set_1of2(pairs)) (
        pairs[i,1] = pairs[j,1] <-> pairs[i,2] = pairs[j,2]
     )
  )
;

% alternative version
predicate twin2(array[int,int] of var int: pairs) =
  forall(i in index_set_1of2(pairs)) (
    not(exists(j in index_set_1of2(pairs)) (
      pairs[i,1] = pairs[j,1] /\
      pairs[i,2] != pairs[j,2]
    ))

    /\
    not(exists(j in index_set_1of2(pairs)) (
      pairs[i,1] != pairs[j,1] /\
      pairs[i,2] == pairs[j,2]
    ))
  )
;



solve satisfy;
% solve :: int_search(x, first_fail, indomain_min, complete) satisfy;

constraint
  % % Original problem:
  % cp2d(pairs, array2d(1..n, 1..2,
  %       [1,8,
  %        9,6,
  %        1,8,
  %        5,0,
  %        6,7,
  %        9,6]))

  % % The pairs (1,8) and (9,6) should be restored
  % cp2d(pairs, array2d(1..n, 1..2,
  %       [1,_,
  %        9,6,
  %        1,8,
  %        5,0,
  %        6,7,
  %        9,_]))

  % Here we also let (5,_) free -> 7 solutions (6,7, and 8 are not possible)
  cp2d(pairs, array2d(1..n, 1..2,
        [_,8,
         9,6,
         1,8,
         5,_,
         6,7,
         9,_]))
;

constraint
  twin(pairs)
;

output [
  if j = 1 then "\n" else " " endif ++
    show(pairs[i,j])  
  | i in 1..n, j in 1..2

];

%
% data
%
% pairs = array2d(1..n, 1..2,
% [1,8,
%  9,6,
%  1,8,
%  5,0,
%  6,7,
%  9,6]);

