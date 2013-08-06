% 
% Magic sequence in MiniZinc.
% Alternative version which use the global constraint distribute.
% See magic_sequence.mzn, magic_sequence2.mzn.
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

int: n =  10;
array[0..n-1] of var 0..n-1: s;
array[0..n-1] of int: value = array1d(0..n-1, [i | i in 0..n-1]); % helper for distribute

% solve satisfy;
solve :: int_search(s, first_fail, indomain_min, complete) satisfy;

constraint
   distribute(s, value, s)         /\ 
   sum(i in 0..n-1) (s[i])   = n   /\
   sum(i in 0..n-1) (s[i]*i) = n
;


output [
       show(s), "\n"
];



