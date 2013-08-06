% 
% Investment problem in MiniZinc.
%  
% This is a MIP version of 
% http://www.hakank.org/minizinc/investment_problem.mzn
% 
% From 
% ORMM (Operations Research Models and Methods):
% http://www.me.utexas.edu/~jensen/ORMM/models/unit/dynamic/subunits/investment/index.html
% """
% A portfolio manager with a fixed budget of $100 million is 
% considering the eight investment opportunities shown in Table 1. 
% The manager must choose an investment level for each alternative 
% ranging from $0 to $40 million. Although an acceptable investment 
% may assume any value within the range, we discretize the 
% permissible allocations to intervals of $10 million to facilitate 
% the modeling. This restriction is important to what follows. 
% For convenience we define a unit of investment to be $10 
% million. In these terms, the budget is 10 and the amounts to 
%invest are the integers in the range from 0 to 4.
%
% Table 1. Returns from Investment Opportunities
%
% Amount      Opportunity
% Invested
% ($10 million)      1   2   3   4   5   6   7   8  
% 0                  0   0   0   0   0   0   0   0
% 1                4.1 1.8 1.5 2.2 1.3 4.2 2.2 1.0
% 2                5.8 3,0 2.5 3.8 2.4 5.9 3.5 1.7
% 3                6.5 3.9 3.3 4.8 3.2 6.6 4.2 2.3
% 4                6.8 4.5 3.8 5.5 3.9 6.8 4.6 2.8
%
% Table 1 provides the net annual returns from the investment 
% opportunities expressed in millions of dollars. A ninth 
% opportunity, not shown in the table, is available for funds 
% left over from the first eight investments. The return is 5% 
% per year for the amount invested, or equivalently, $0.5 million 
% for each $10 million invested. The manager's goal is to 
% maximize the total annual return without exceeding the budget.
%
% The investment problem has a general mathematical programming 
% formulation.
%
%    Maximize z = r1(x1)+r2(x2)+...rm(xm)+e*y
%    subject to: x1+x2..xm+y=b
%        0 <= xj <= uj and integer for j = 1..m
%        y>= 0 and integer
%
% The notation is the general model is defined below.
%     xj: amount invested in opportunity j
%     rj(xj): return of opportunity j as a function of xj
%             The returns are given in Table 1 
%     uj: upper bound on investment j, an integer
%     b: the budget for investments, an integer
%     y: unspent money
%     e: return for unspend money

% The problem as stated is similar in structure to the knapsack 
% problem but the objective function is nonlinear. To formulate it 
% as a mixed-integer linear program it would be necessary to 
% introduce 32 binary variables, one for each nonzero level of 
% investment. Since the budget and investment amounts are integer 
% the slack variable, y, can be treated as integer. Rather than 
% pursuing the MILP formulation we will use the problem as an 
% introduction to dynamic programming.
% """
%
% The answer according to
% http://www.me.utexas.edu/~jensen/ORMM/models/unit/dynamic/subunits/investment/dyn_finish.html
% """
% The investor should invest $10 unit each in opportunities 2, 3, 5, 
% and 7; invest $20 million in opportunities 1, 4 and 6; and make 
% no investment in opportunity 8. The entire budget has been 
% spent and there is no slack. The total annual return is $22.3 
% million.
% """

% This model, using solve satisfy, gives the following these 
% two solutions:
%
% not_invested = 0
% the_returns  = [58, 18, 15, 38, 0, 59, 35, 0]
% x            = [ 2,  1,  1,  2, 0,  2,  2, 0]
% z            = 223
%
% not_invested = 0
% the_returns  = [58, 18, 15, 38, 13, 59, 22, 0]
% x            = [ 2,  1,  1,  2,  1,  2,  1, 0]
% z            = 223
%
% The first is an alternative solution:
% 10 million in opportunities 2, and 3, 
% 20 milion in opportunities 1,4,6,7
%  0 in opportunity 8.

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

% include "globals.mzn"; 
int: n = 4;
int: m = 8;
array[0..n,1..m] of int: returns;

array[0..n, 1..m] of var 0..1: x; % x, MIP version
array[1..m] of var 0..n: choices; % the choices
array[1..m] of var int: the_returns;

int: budget = 10; % total 10 million
int: not_invested_returns = 5; % return for not invested (0.5*10)
var 0..budget: not_invested;
var 0..1000: z;


% solve satisfy;
% solve maximize z;
solve :: int_search(
        [x[i,j] | i in 0..n, j in 1..m],
        smallest, 
        indomain_min,
        complete) 
    maximize z;
    % satisfy;    

constraint
    forall(j in 1..m) (
       sum([x[i,j] | i in 0..n])  <= 1
    ) 
    /\
    % z >= 223 /\ % for solve satisfy
    z = sum(i in 0..n, j in 1..m, ) (
          returns[i,j]*x[i,j]
        ) + not_invested*not_invested_returns
    /\
    sum(i in 0..n, j in 1..m) (i*x[i,j])+not_invested <= budget

    /\
    forall(j in 1..m) (
       the_returns[j] >= 0 /\
       the_returns[j] = sum([returns[i,j]*x[i,j] | i in 0..n]) /\
       choices[j] = sum([i*x[i,j] | i in 0..n])
    )
;

% multiplied with 10
returns = array2d(0..n,1..m, [
 0, 0, 0, 0, 0, 0, 0, 0, % 0
41,18,15,22,13,42,22,10, % 1
58,30,25,38,24,59,35,17, % 2
65,39,33,48,32,66,42,23, % 3
68,45,38,55,39,68,46,28  % 4
]);


output
[
 "x: " ++ show(x) ++ "\n" ++
 "choices: " ++ show(choices) ++ "\n" ++
 "the_returns: " ++ show(the_returns) ++ "\n" ++
 "not_invested: " ++ show(not_invested) ++ "\n" ++
 "z: " ++ show(z) ++ "\n"

];
