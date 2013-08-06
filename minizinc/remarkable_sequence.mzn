%
% Remarkable sequence in Minizinc.
% 
% Problem statement in the Alma-0 example program remarkable.a0
% """
% This problem is taken from
% @book{CC88,
% author = "H. Coelho and J. C. Cotta",
% title = "{P}rolog by Example",
% publisher = "Springer-Verlag",
% address = "Berlin",
% year = 1988
% }
% (page 193)
%
% Call a sequence of 27 elements remarkable if it consists of three 1's,
% three 2's, ...  three 9's arranged in such a way that for all i in
% [1..9] there are exactly i numbers between successive occurrences of
% i.  For example, the sequence
%
% (1,9,1,2,1,8,2,4,6,2,7,9,4,5,8,6,3,4,7,5,3,9,6,8,3,5,7)
%
% is remarkable.  Write a program that generates all
% remarkable sequences.
% """
%
% Model created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

include "globals.mzn";

int: n = 9; % the digits
int: m = 3; % number of occurrences of each number
array[1..n*m] of var 1..n: a;

solve :: int_search(a, first_fail, indomain, complete) satisfy;

constraint
  forall(i in 1..n) (
     exists(j in 1..25-(2*i)) (
       a[j] = i /\
       a[j+i+1] = i /\
       a[j+2*i+2] = i
     )
  )
  /\ 
  % exact 3 occurrences of each digit
  forall(i in 1..n) (
    count(a, i, m)
  )
  /\ % Symmetry breaking: First element is less than the last
  a[1] <= a[n*m]
    
;

output [
  show(a)
];
