% 
% Scheduling example (Bratko) in MiniZinc.
% 
% Scheduling example 2 from 
% Ivan Bratko "Prolog - Programming for Artificial Intelligence", 3rd edition,
% page 329ff.
%
 
% Note: MiniZinc gives only numeric solutions. The solution at page 332
%    FinTime = 22
%    [t1/0/5, t2/5/7, t3/12/10, t4/S4/2, t5/S5/9]  ([id/start/length, ...])
%    S5 <= 13
%    S4 >= 5
%    S4-S5 <= -2
% 
% uses CLP(R) which is numeric combined with symbolic result.
% This MiniZinc cannot handle.


% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

include "globals.mzn";

int: n = 5;
array[1..n] of int: Ds = [5, 7, 10, 2, 9]; % Durations
array[1..n] of int: Rs = [1, 1,  1, 1, 1]; % Resource

int: num_prec = 4; % number of precedences
array[1..num_prec, 1..2] of 0..n: prec; % precendences

var 0..1000: Capacity;
array[1..n] of var 0..30: Ss;
var 0..50: End;

% solve satisfy;
solve minimize End;


predicate after(array[int] of var int: S, array[int] of var int: D, var int: E) =
   forall(i in index_set(S)) (
      E >= S[i] + D[i]
   )
;

% handle precendences
predicate after_prec(var int: a, var int: b, var int: a_time) =
   b >= a + a_time
;

constraint
    Capacity >= 1 /\
    after(Ss, Ds, End) /\
    cumulative(Ss, Ds, Rs, Capacity)
    /\
    forall(i in 1..num_prec) (
       after_prec(Ss[prec[i,1]], Ss[prec[i,2]], Ds[prec[i,1]])
    )
;

% precedences
prec = array2d(1..num_prec, 1..2, [
   1,2,
   1,4,
   2,3,
   4,5,
]);

output [
  "Ds: ", show(Ds), "\n",
  "Rs: ", show(Rs), "\n",
  "Ss: ", show(Ss), "\n",
  "End: ", show(End), "\n",
  "Capacity: ", show(Capacity), "\n",
];