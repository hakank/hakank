% Minizinc model of Capital budgeting
%
%   Winston Operations Research, page 478: Capital budgeting 
%
%   and some extra constraint (page 479):
%
%   either one of: 
%    - can only make two investments
%    - if investment 2 then investment 1
%    - if investment 2 then not investment 4
%
% Model created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc

set of 1..4: investments = 1..4;    % number of investments
array[investments] of  var 0..1: x; % 1 if investment i
array[investments] of int: npv;     % Net Present Value
int: budget; % the budget
array[investments] of int: cash_flow; % the cash_flow for each investment

% the objective to maximize:
% the sum of all returns for each selected investments
var int: z = sum(i in investments) (x[i]*npv[i]);

solve maximize z;
% solve satisfy;

constraint
       budget >= 0 % sanity check
       /\ % cash_flow must be >= 0
       forall(i in investments) (cash_flow[i] >= 0) 

      /\ % the sum of all choosen investments must be less than the budget
      sum(i in investments) (x[i]*cash_flow[i]) <= budget
%        % for solve satisfy
%       /\ z >= 38
;

%
% the extra constraints
%

% only two investments
% constraint
%        sum(i in investments) (x[i]) = 2
% ;

% if investment 2 -> investment 1
% constraint
%      x[2] = 1 -> x[1] = 1 % the minizinc way
%      %  x[1] >= x[2]      % alternative (integer programming) way 
% ;

% if investment 2 then not investment 4
% constraint 
%     x[2] = 1 -> x[4] = 0 % minizinc way
%     %   x[4] + x[2] <= 1
%;

%
% data
%
budget = 14;
npv = [16,22,12,8];
cash_flow = [5,7,4,3];


output
[ 

  "x: " ++ show(x) ++ "\n" ++
  "z: " ++ show(z)
];