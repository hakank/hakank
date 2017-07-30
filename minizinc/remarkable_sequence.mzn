%
% Remarkable sequence (a.k.a. Langford's number problem) in Minizinc.
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
% Note: This is a generalized version of Langford's Number problem (CSPLib #24)
%
% Model created by Hakan Kjellerstrand, hakank@gmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc/
%

include "globals.mzn";

int: n = 15; % the digits
int: m = 3; % number of occurrences of each number
array[1..n*m] of var 1..n: a;

array[1..n] of var m..m: gcc;

solve :: int_search(a, max_regret, indomain_split, complete) satisfy;

constraint
  assert(n mod m == 0, "n (" ++ show(n) ++ ") must be a multiple of m (" ++ show(m) ++ ")")
  
;

constraint
  %% for n=9, m=3
  % forall(i in 1..n) (
  %    exists(j in 1..25-(2*i)) (
  %      a[j] = i /\
  %      a[j+i+1] = i /\
  %      a[j+2*i+2] = i
  %    )
  % )

  forall(i in 1..n) (
    let {
       var 1..n*m - ((m-1)*i): j;
    } in 
    % exists(j in 1..n*m - ((m-1)*i)) (
       forall(k in 0..m-1) (
         a[j+(i*k)+k] = i 
       )
    % )
  )

  /\
  global_cardinality(a, [i | i in 1..n], gcc) % :: domain
  /\ % Symmetry breaking
  a[1] <= a[n*m]
    
;

output [
  show(a)
];
