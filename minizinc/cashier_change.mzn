% 
% Cashier change problem in MiniZinc.
% 
% From "Puzzle-based learning", page 86ff
% """
% A customer tried to change a dollar in a shop. The cashier said:
% "I am sorry, but I can not change it with the coins I have."
% The customer then asked for a change of half dollar, but the cashier
% replied "I am sorry, but I can not change it with the coins I have."
% The customer asked for a change of a quarter, then a dime, then 
% a nickel, but for all these requests the cashier kept replying
% "I am sorry, but I can not change it with the coins I have." Finally,
% the customer got upset and asked: "Do you have any coins at all?"
% The casher replied: "Yes ... I have $1.15 in coins." The question
% is: what coins where in the cash register?
% """

% Nomenclature:
% - half dollar: 50 cent
% - quarter    : 25 cent
% - dime       : 10 cent
% - nickel     :  5 cent
% -            :  1 cent

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@gmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc/
%

include "globals.mzn"; 


int: total = 115; % $1.15
int: n = 5;
array[1..n] of int: b = [1,5,10,25,50];
array[int] of int: b2 = b ++ [100];

% decision variables
array[1..n] of var 0..total: x;

% solve satisfy;
solve :: int_search(x, first_fail, indomain_split, complete) satisfy;

constraint
  sum([x[i]*b[i] | i in 1..n]) = total /\
 
  % for all denominations b[i]:
  forall(i in 1..n) (
    % and for all the denominations larger than b[i]:

    % forall(j in i+1..n+1) (
    %   % there is no amount of b[i]*(1..x[i]) that can yield the amount of b2[j]
    %   not(exists(k in 1..ub(x[i])) (
    %     k <= x[i] /\
    %     b[i]*k = b2[j]
    %   ))
    % )

    % using sum instead
    forall(j in i+1..n+1) (
      sum([ bool2int(k <= x[i] /\ b[i]*k = b2[j]) | k in 1..ub(x[i])]) = 0      
    )

  ) 
;

output [
  show(x), "\n",
  "sum(x): ", show(sum([x[i]*b[i] | i in 1..n])), "\n"
]
++
[
  show(b[i]) ++ " cent: " ++ show(x[i]) ++ "\n"
  | i in 1..n
]
;

