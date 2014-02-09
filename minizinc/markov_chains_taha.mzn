% 
% Markov Chains in MiniZinc.
%
% From Hamdy Taha "Operations Research" (8th edition), page 649ff.
% Fertilizer example.
% 

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc .

% include "globals.mzn"; 

int: n = 3;
array[1..n, 1..n] of float: mat;
array[1..n] of var 0.0..1.0: p; % probabilities
array[1..n] of var 0.0..1.0: mean_first_return_time;
array[1..n] of float: cost; % cost, page 651
var 0.0..1000.0: tot_cost;


% solve satisfy;
solve :: float_search(p++mean_first_return_time, 0.0001, input_order, indomain_split, complete) satisfy;

%
% Calculates the steady state probablity of a transition matrix m
%
predicate steady_state_prob(array[int, int] of float: m, array[int] of var float: prob) =
   let {
     int: len = card(index_set_1of2(m))
   }
   in
    forall(i in 1..len) (
     prob[i] = sum(j in 1..len) (prob[j]* m[j,i])
   )
   /\
   sum(i in 1..n) (prob[i]) = 1.0
;

%
% calculate the mean first return time from a steady state probability array
%
predicate get_mean_first_return_time(array[int] of var float: prob, array[int] of var float: mfrt) =

   forall(i in 1..card(index_set(prob))) (
     % Note: As of writing (20080710), neither MiniZinc/flatzinc nor Gecode/fz
     % can handle float_div (or float_mult). ECLiPSe ic solver can handle it, though.
     mfrt[i] = 1.0/prob[i]
   )

; 

constraint

   steady_state_prob(mat, p)
   %/\
   %get_mean_first_return_time(p, mean_first_return_time)
   /\
   tot_cost = sum(i in 1..n) (cost[i]*p[i])
;


%
% data
%
mat = array2d(1..n, 1..n,
[
  0.3, 0.6,  0.1,
  0.1, 0.6,  0.3,
 0.05, 0.4, 0.55
]);

% the transition matrix page 650
% mat = array2d(1..n, 1..n,
% [
%    0.35, 0.6,  0.05,
%    0.3, 0.6,  0.1,
%   0.25, 0.4, 0.35
% ]);



% page 651
cost = [100.0, 125.0, 160.0];


output [
"p: " ++ show(p) ++ "\n"++
"mean_first_return_time: " ++ show(mean_first_return_time) ++ "\n"++
"tot_cost: " ++ show(tot_cost) ++ "\n"
];
