% 
% Enigma puzzle Five fives (#1358) in MiniZinc.
% 
% Problem formulation from
% http://www.f1compiler.com/samples/Enigma%201358.f1.html
% """
%  Five fives
% 
%  Enigma 1358 Adrian Somerfield, New Scientist magazine, September 17, 2005.
% 
%  You might think there is something wrong with the addition sum shown below, 
%  but in fact each of the five numbers shown in the sum is in a different base.
%  
%  1 1 1 0 1
%  1 1 1 0 1
%  1 1 1 0 1
%  1 1 1 0 1
%  ---------
%  1 1 1 0 1
%  
%  All five numbers are even and the given total at the bottom is less 
%  than 100,000.
%  
%  What is that total? 
%
% """

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http:// www.hakank.org/minizinc
%

include "globals.mzn"; 

int: n = 5;
set of int: num_bases = 2..20;
array[1..n] of var num_bases: bases;
array[1..n] of var 0..100000: x;


predicate toNum(array[int] of var int: tal, var int: summa, float: base) =
    let { int: len = length(tal) }
    in
    summa = sum(i in 1..len) (
       ceil(pow(base, int2float(len-i))) * tal[i]
    )
    /\ forall(i in 1..len) (tal[i] >= 0)
;


solve satisfy;
% solve :: int_search(bases, "first_fail", "indomain", "complete") satisfy;

constraint
  % find the bases
  forall(i in 1..n) (
     exists(b in num_bases) (
        bases[i] = b
        /\
        toNum([1,1,1,0,1], x[i], int2float(b))
     )
     /\
     x[i] mod 2 = 0
  )

  /\ % bases are different
  all_different(bases)

  /\ % calculate the sum
  x[n] = sum(i in 1..n-1) (x[i])

  /\ % symmetry breaking
  increasing(bases)  
;

output [
  "x: ", show(x), "\n",
  "bases: ", show(bases), "\n"

];
