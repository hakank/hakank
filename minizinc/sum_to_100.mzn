%
% Number puzzle in Minizinc
% 

% From comp.lang.prolog 
% http://groups.google.se/group/comp.lang.prolog/browse_thread/thread/fe06d673dfbaafce/c0b02c9caeed60cc?lnk=st&q=eclipse++%22lib(ic%22)+constraint+programming&rnum=2&hl=sv#c0b02c9caeed60cc

% """
% Recently I was trying to figure out the most efficient way
% of answering someone's question about finding all combinations of 
% five numbers which add to 100, given that each number must 
% be divisible by 2 or 5 (no, I don't know why he needed that either.)
% """
%
% Model created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

include "globals.mzn";

array[1..5] of var 1..100: x;
solve :: int_search(x, first_fail, indomain_min, complete) satisfy;
% solve satisfy;

constraint
  sum(i in 1..5) (x[i]) = 100 /\
  forall(i in 1..5) (
      x[i] mod 2 = 0 \/ x[i] mod 5 = 0
  )
  /\ % symmetry breaking
  increasing(x)
;

output
[
  show(x)
];