%
% Gray codes in Minizinc
%
%
%
% This Minizinc program is written by Hakan Kjellerstrand, hakank@bonetmail.com,
% and is commented in the (swedish) blog post
% Constraint Programming: Minizinc, Gecode/flatzinc och ECLiPSe/minizinc
% http://www.hakank.org/webblogg/archives/001209.html
%
% See also my MiniZinc page: http://www.hakank.org/minizinc
%


include "globals.mzn";

int: n = 4; % n is number of bits, e.g. n=4 -> 0..2^n-1, i.e. 0..15
int: m = ceil(pow(2.0, int2float(n))); % the amount of numbers in the array
array[1..m] of var 0..m: x;
array[1..m, 1..n] of var 0..1: binary;
array[1..m] of var 0..n: num_bits;

solve :: int_search(x , first_fail, indomain_split, complete) satisfy;
% solve satisfy;

predicate toNum(array[int] of var int: tal, var int: summa,  float: base) =
          let { int: len = length(tal) }
          in
          summa = sum(i in 1..len) (
            ceil(pow(base, int2float(len-i))) * tal[i]
          )
          /\ forall(i in 1..len) (tal[i] >= 0)
;


constraint
    % symmetry breaking
     x[1] = 0 % first should be 0 (symmetry breaking)
    /\
    sum(i in 1..n) (binary[m,i]) = 1 % last should be 1
    /\
    all_different(x) 

    /\ % convert x <-> binary (just for all_different(x)
    forall(i in 1..m) (
       let {
          array[1..n] of var 0..1: t
       }
       in
       toNum(t, x[i], 2.0) /\
       forall(j in 1..n) (
         binary[i,j] = t[j]
       )
    )
    /\
     % Gray Code: there must be exactly one bit difference between two numbers
     forall(i in 2..m) (
        sum(j in 1..n) ( bool2int( binary[i, j] != binary[i-1, j] ) ) = 1 
     ) 
     /\ % for num_bits-vektorn
     forall(i in 1..m) (
       num_bits[i] = sum(j in 1..n) (binary[i,j])
     )
;


output [
   "\nx: " ++ show(x) ++
   "\nnum_bits: " ++ show(num_bits), "\n"
]
