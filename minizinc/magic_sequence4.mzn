% 
% Magic sequence in MiniZinc.
%
% Alternative version which use the global constraint global_cardinality.
% See magic_sequence.mzn, magic_sequence2.mzn, magic_sequence3.mzn.
% 
% http://www.dcs.st-and.ac.uk/~ianm/CSPLib/prob/prob019/spec.html
% """
% A magic sequence of length n is a sequence of integers x0 . . xn-1 between 0 and n-1, such that for all i in 0 to n-1, the number i occurs exactly xi times in the sequence. For instance, 6,2,1,0,0,0,1,0,0,0 is a magic sequence since 0 occurs 6 times in it, 1 occurs twice, ...
% """
% 
% Model created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

include "globals.mzn";

int: n =  100;
array[0..n-1] of var 0..n-1: s;

% solve satisfy;
solve :: int_search(s, first_fail, indomain_min, complete) satisfy;

constraint
   global_cardinality(s,array1d(0..n-1, set2array(index_set(s))), s) :: domain
;


output [
       show(s), "\n"
];



