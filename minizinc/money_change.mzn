% 
% Simple money change problem in MiniZinc.
% 
% 
% 
% Model created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

int: n;     % number of coins
int: value; % value to change
array[1..n] of int: coins; % the values of the coins
array[1..n] of int: prio;  % prioritize some coins. Larger value better.
array[1..n] of var int: x;

var int: num_used_coins = sum(x);

%
% Some different solve options.
%

solve satisfy;

% Minimize the number of coins
% solve minimize num_used_coins;

% Use the prio array to optimize
% solve maximize sum(i in 1..n) (x[i]*prio[i]);

constraint
  % 0 or more number of coins
  forall(i in 1..n) (
    x[i] >= 0
  )
  /\
  sum(i in 1..n) (x[i]*coins[i]) = value

;

%
% data
%
% n = 6;
% coins = [50,25,10,5,2,1];
% prio = [1,2,3,4,5,6];
% value = 100;

n = 6;
coins = [100,50,25,10,5,1];
prio = [1,2,3,4,5,6];
value = 100;



output
[
  "x: " ++ show(x) ++ "\n" ++
  "num_used_coins: " ++ show(num_used_coins) ++ "\n" 
];