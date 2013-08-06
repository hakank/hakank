%
% Coin application in Minzinc
%
% 
% From "The ECLiPSe Book" pages 99f and 234 ff
% The solution in ECLiPSe is at page 236.

% """
% What is the minimum number of coins that allows one to pay _exactly_
% any amount smaller than one Euro? Recall that there are six different
% euro cents, of denomination 1, 2, 5, 10, 20, 50
% """
%
% Model created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc

include "globals.mzn";

% the original problem (page 99)
int: n = 6; % number of different coins
array[1..n] of int: variables = [1, 2, 5, 10, 25, 50]; 

% alternative problems
% int: n = 7; % 
% array[1..n] of 1..99: variables = [1, 5, 10, 25, 50, 100];
% array[1..n] of var 1..99: variables; % = [1, 2, 4, 8, 16, 32, 64];

array[1..n] of var 0..99: x; % array for the changes
var int: num_coins = sum(i in 1..n) (x[i]);  % number of coins used


% solve satisfy;
solve :: int_search(x, first_fail, indomain_min, complete) minimize num_coins;
% solve minimize variables[n];
% solve minimize sum(i in 1..n) (variables[i]); 


constraint
        % symmetry breaking for the elaborate case when variables is not initiated
        forall(i in 2..n) (
          variables[i-1] < variables[i] 
        )
        /\ 
        all_different(variables) 
        /\
        sum(i in 1..n) (variables[i]) <= 99 
        /\
        % must be at least 0 coins
        forall(i in 1..n) (
          x[i] >= 0
        )
        /\ % This is the "main loop"        
           % Checks that all changes from 1 to 99 can be made
           %
        forall(j in 1..99) (
          let {
              array[1..n] of var 0..99: tmp} 
          in
          sum(i in 1..n) (tmp[i]*variables[i]) = j /\
            forall(i in 1..n) (
              tmp[i] <= x[i]
          )
        )

;


output [
  show(x), "\n",
];