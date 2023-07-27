% 
% Period constraint in MiniZinc.
% 
% Global constraint period
% https://sofdem.github.io/gccat/gccat/Cperiod.html
% """
% Let us note V0, V1, ..., Vm−1 the variables of the VARIABLES collection. 
% PERIOD is the period of the sequence V0 V1...Vm−1 according to constraint  
% CTR . This means that PERIOD is the smallest natural number such that 
% Vi CTR Vi+PERIOD holds for all i∈0, 1, ..., m−PERIOD−1.
% ...
% Example
%  period(3, [1,1,4,1,1,4,1,1]
% 
% The period constraint holds since, as depicted by Figure 4.230.1, its first 
% argument PERIOD=3 is equal (i.e., since CTR is set to =) to the period of 
% the sequence 1 1 4 1 1 4 1 1.
% """
% 
% Model created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc

include "globals.mzn"; 

int: n;

% the period length. Cannot be a var int since p is used in ranges.
int: per_len;

array[1..n] of var 1..n: x;
array[1..per_len] of var 1..n: the_period;

predicate cp1d(array[int] of var int: x, array[int] of var int: y) =
  assert(index_set(x) = index_set(y),
           "cp1d: x and y have different sizes",
     forall(i in index_set(x)) ( x[i] = y[i] ))
; 


solve satisfy;

predicate period(int: p, array[int] of var int: x) =
   let {
     int: len = length(x),
     int: left = len - p*(len mod p)
   }
   in
   assert(p <= len div 2, "p must be less than or equal to len(x) div 2",

     % check that there are (len mod p) "full" periods
     forall(i in 0..(n div p)-2) (
        cp1d([x[j] | j in 1+(p*i)..1+(p*i)+p-1], [x[k] | k in 1+(p*(i+1))..1+(p*(i+1))+p-1])
     )
     /\ 
     % if len mod p != 0 then we must assure that the last
     % m = len - p*(len mod p) elements 
     % is the same as the first m elements (in the period)
     if left > 0 then
        forall(i in 1..left) (
          x[i] = x[p*(len mod p)+i]
        )
     else 
       true
     endif

   ) % end assert
;

constraint
   % cp1d(x,[1,1,4,  1,1,4,  1,1]) % has a period of 3
   % cp1d(x, [1,2, 1,2, 1,2, 1,3]) % no period
   % cp1d(x, [1,2, 1,2, 1,2, 1,2]) % has a period of 2
   % /\
   period(per_len, x)
   /\
   forall(i in 1..per_len) (
      the_period[i] = x[i]
   )

   % /\ % and add some constraints on the period
   % all_different([x[i] | i in 1..per_len])
   % /\
   % not increasing([x[i] | i in 1..per_len])
;


%
% data
%

n = 8;
per_len = 3;

output
[
  "x: " ++ show(x) ++ "\n" ++
  "the_period: " ++ show(the_period)
];
