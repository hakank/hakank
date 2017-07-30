% 
% Magic sequence in MiniZinc.
% Alternative version, see magic_sequence.mzn.
% 
% http://www.dcs.st-and.ac.uk/~ianm/CSPLib/prob/prob019/spec.html
% """
% A magic sequence of length n is a sequence of integers x0 . . xn-1 between 0 and n-1, such that for all i in 0 to n-1, the number i occurs exactly xi times in the sequence. For instance, 6,2,1,0,0,0,1,0,0,0 is a magic sequence since 0 occurs 6 times in it, 1 occurs twice, ...
% """
% 
% Model created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

int: n =  100;
array[0..n-1] of var 0..n-1: s;

solve satisfy;

constraint
        forall(i in 0..n-1) (
                s[i] = sum(j in 0..n-1) (bool2int(s[j]=i))
        )
        /\ 
        sum(i in 0..n-1) (s[i])=n 
        /\
        sum(i in 0..n-1) (s[i]*i) =n
;


output [
       show(s), "\n",
];



