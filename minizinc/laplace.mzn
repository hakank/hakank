% 
% Laplace problem in MiniZinc.
% 
% From the CLP(R) laplace example:
% """
% *************************************
% CLP(R) Version 1.1 - Example Programs
% *************************************
%
% Solve the Dirichlet problem for Laplace's equation using
% Leibman's five-point finit-differenc approximation. 
% The goal ?- go1 is a normal example, while the goal ?- go2
% shows output constraints for a small region where the boundary conditions
% are not specified.
% """
% 
% Notes: 
%   - This is a MIP problem. Not many FlatZinc solver can solve it. Here's
%     the one that give at least some correct example:
%      * G12/mip
%      * ECLiPSe/eplex (MIP solver)
%      * Gecode (kind of, since it returns the same solution "infinitely")
%     
%   - I later realized that the MiniZinc distribution has a model
%     of this problem, 
%       tutorial/laplace.mzn
%     though with a different setup of the problem instance.
%

% This MiniZinc model was created by Hakan Kjellerstrand, hakank@gmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc/
%

% include "globals.mzn"; 

int: r;
int: c;
float: Z; % zero
float: M; % max value

% decision variables
array[0..r, 0..c] of var Z..M: x;


% solve satisfy;
solve :: float_search (
        [x[i,j] | i in 0..r, j in 1..c], 
        0.0001, 
        input_order, 
        indomain_reverse_split, 
        complete) 
    % satisfy;
    minimize x[1,1]; % To keep Gecode/fz happier.

constraint
   forall(i in 1..r-1, j in 1..c-1) (
      4.0*x[i,j] = x[i-1,j] + x[i,j-1] + x[i+1,j] + x[i,j+1]
   )
;

output [
  if j = 0 then "\n" else " " endif ++
    show_float(8,2, x[i,j])
  | i in 0..r, j in 0..c
];


% Instance from CLP(R) laplace example
r = 10;
c = 10;
Z = 0.0;
M = 100.0;
x = array2d(0..r,0..c, 
   [
    Z,Z,Z,Z,Z,Z,Z,Z,Z,Z,Z,
    M,_,_,_,_,_,_,_,_,_,M,
    M,_,_,_,_,_,_,_,_,_,M,
    M,_,_,_,_,_,_,_,_,_,M,
    M,_,_,_,_,_,_,_,_,_,M,
    M,_,_,_,_,_,_,_,_,_,M,
    M,_,_,_,_,_,_,_,_,_,M,
    M,_,_,_,_,_,_,_,_,_,M,
    M,_,_,_,_,_,_,_,_,_,M,
    M,_,_,_,_,_,_,_,_,_,M,
    M,M,M,M,M,M,M,M,M,M,M
   ]);


%% The problem instance from MiniZinc's tutorial/laplace.mzn
% Z = 0.0;
% M = 100.0;
% r = 4;
% c = 4;
% x = array2d(0..r, 0..c,
%   [
%   Z,M,M,M,Z,
%   Z,_,_,_,Z, 
%   Z,_,_,_,Z, 
%   Z,_,_,_,Z,
%   Z,Z,Z,Z,Z
%   ]);
