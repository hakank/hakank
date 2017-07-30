% 
% Portfolio optimization in MiniZinc.
% 
% From Xpress-Model example foliolp.mos
% http://examples.xpress.fico.com/example.pl?id=mosel_model_10

% G12/mip solves this easy:
%   frac:[30, 0, 20, 0, 7, 30, 0, 0, 13, 0]
%   return: 1399
%
% as does fzn2smt
% 
% The CP solvers has a little more problem...

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc/
%

% include "globals.mzn"; 

set of int: SHARES = 1..10;  % set of shares
set of int: RISK = {2,3,4,9,10}; % set of high-risk values among shares
set of int: NA = {1,2,3,4}; % set of shares issues in N.-America

% estimated return in investment
array[SHARES] of int: ret = [5,17,26,12,8,9,7,6,31,21]; 

% decision variables
% Note: since we use integers, we multiply with 100 (%)
array[SHARES] of var 0..100: frac;
var 0..sum(ret)*100: return = sum(s in SHARES) (ret[s]*frac[s]);

% solve maximize return;
solve :: int_search(frac, anti_first_fail, indomain_max, complete) maximize return;

constraint
   %  Limit the percentage of high-risk values
   sum(s in RISK) (frac[s]) <= 33 % 1/3
   /\
   % Minimum amount of North-American values
   sum(s in NA) (frac[s]) >= 50 % 0.5
   /\
   % Spend all the capital
   sum(s in SHARES) (frac[s]) = 100
   /\
   % Upper bounds on the investment per share
   forall(s in SHARES) (
      frac[s] <= 30 % 0.3
   )

;

output [
  "frac:" ++ show(frac) ++ "\n" ++
  "return: " ++ show(return)
]
 ++ ["\n"]
;

