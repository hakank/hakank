% 
% 
% Enigma 1224 - Age-changing in MiniZinc.

% From Enigma 1224: Age-changing
% https://enigmaticcode.wordpress.com/2015/06/20/enigma-1224-age-changing/
% """
% From New Scientist #2380, 1st February 2003
%
%   If you start with my age, in years, and apply the four operations:
%
%   [  
%      +2  /8 
%
%      -3  *7
%
%   ]
%
%   in some order, then the final answer you get is my husband’s age in years.
%
%   Funnily enough, if you start with his age and apply the same four operations in a 
%   different order, then you get my age.
%
%   What are our two ages?
% """

% Note: There are just few FlatZinc solvers that can handle these kind of MIP models:
%   - Gecode: 23ms
%   - JaCoP: 623ms
%   - G12 mip: 6s (shows only one solution)
%

%
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@gmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc/
%

include "globals.mzn"; 

int: n = 4;
set of int: ages = 16..120;

% decision variables
var ages: m; % my age
var ages: h; % husbands age

array[1..n] of var 1..n: perm1;
array[1..n] of var 1..n: perm2;

array[1..n+1] of var -1000.0..1000.0: hlist; % calculating husbands age
array[1..n+1] of var -1000.0..1000.0: mlist; % for checking my age

% solve satisfy;
solve :: int_search(perm1 ++ perm2, first_fail, indomain_split, complete) satisfy;


predicate check(var int: perm, var float: old, var float: new) =
   if     perm = 1 then new = old + 2
   elseif perm = 2 then new = old / 8
   elseif perm = 3 then new = old - 3
   elseif perm = 4 then new = old * 7
   else true
   endif
;


constraint
  % find the two (different) permutations of operators
  all_different(perm1) /\
  all_different(perm2) /\
  sum([perm1[i] != perm2[i] | i in 1..n]) > 0
;

constraint

  % find husbands age
  % start with my age
  hlist[1] = m 
  % husband's age is last in hlist
  /\ h = hlist[n+1] 

  % checking my age, start with husband's age
  /\ mlist[1] = h
  % my age is last in mlist
  /\ m = mlist[n+1]
  /\
  forall(i in 1..n) (
    check(perm1[i], hlist[i], hlist[i+1]) /\
    check(perm2[i], mlist[i], mlist[i+1])
  )

  % Symmetry breaking: I'm younger than husband
  % (-> 5ms for Gecode)
  % /\ m <= h
;

output 
[ 
  "my age: ", show(m), "\n",
  "husband's age: ", show(h), "\n",

];

