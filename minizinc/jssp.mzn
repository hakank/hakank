% 
% Job-Shop Scheduling Problem in MiniZinc.
% 
% From GLPK:s example jssp.mod
% """
% JSSP, Job-Shop Scheduling Problem */
%
% Written in GNU MathProg by Andrew Makhorin <mao@mai2.rcnet.ru>
%
% The Job-Shop Scheduling Problem (JSSP) is to schedule a set of jobs
% on a set of machines, subject to the constraint that each machine can
% handle at most one job at a time and the fact that each job has a
% specified processing order through the machines. The objective is to
% schedule the jobs so as to minimize the maximum of their completion
% times.
%
% Reference:
% D. Applegate and W. Cook, "A Computational Study of the Job-Shop
% Scheduling Problem", ORSA J. On Comput., Vol. 3, No. 2, Spring 1991,
% pp. 149-156.
% """

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%
% number of jobs 
int: n;

% number of machines 
int: m;

% set of jobs 
set of int: J = 1..n;

% set of machines 
set of int: M = 1..m;


% permutation of the machines, which represents the processing order
%   of j through the machines: j must be processed first on sigma[j,1],
%   then on sigma[j,2], etc. 
array[J, M] of M: sigma;

% processing time of j on a 
array[J, M] of int: p;

% starting time of j on a 
array[J,M] of var int: x;

% Y[i,j,a] is 1 if i scheduled before j on machine a, and 0 if j is
% scheduled before i
array[J,J,M] of var 0..1: Y;

% some large constant 
var int: K = sum(j in J, a in M) (p[j,a]);

% so-called makespan 
var int: z;


% the objective is to make z as small as possible 
solve :: int_search(
        [x[i,j] | i in J, j in M] ++ 
        [Y[i,j,a] | i,j in J, a in M] ++ [K, z],
        first_fail, % occurrence,
        indomain_min,
        complete % 13 seconds with ECLiPSe/fd
        %"credit(1000, bbs(5))" % takes about 4 seconds with ECLiPSe/ic and /fd
        ) minimize z;

constraint
  % added K as upper limit of z and x
  z >= 0
  /\
  z <= K
  /\
  forall(i in J, a in M) (
       x[i,a] >= 0
       /\
       x[i,a] <= K
  )

  /\

  % sigma must be permutation 
  forall(j in J, t1 in 1..m, t2 in 1..m where t1 != t2) (
        sigma[j,t1] != sigma[j,t2]
  )

  /\
  % x[i,a] >= x[j,a] + p[j,a] iff Y[i,j,a] is 0 
  forall(i in J, j in J, a in M where i != j) (
      x[i,a] >= x[j,a] + p[j,a] - K * Y[i,j,a]
  )

  /\
  % x[j,a] >= x[i,a] + p[i,a] iff Y[i,j,a] is 1 
  forall(i in J, j in J, a in M where i != j) (
      x[j,a] >= x[i,a] + p[i,a] - K * (1 - Y[i,j,a])
  )

  /\
  % j can be processed on sigma[j,t] only after it has been completely
  %   processed on sigma[j,t-1] 
  %
  % The disjunctive condition that each machine can handle at most one
  %   job at a time is the following:
  %
  %      x[i,a] >= x[j,a] + p[j,a]  or  x[j,a] >= x[i,a] + p[i,a]
  %
  %   for all i, j in J, a in M. This condition is modeled through binary
  %   variables Y as shown below. 
  %
  % Y[i,j,a] is 1 if i scheduled before j on machine a, and 0 if j is
  %   scheduled before i 
  forall(j in J, t in 2..m) (
      x[j, sigma[j,t]] >= x[j, sigma[j,t-1]] + p[j, sigma[j,t-1]]
  )

  /\
  % which is the maximum of the completion times of all the jobs 
  forall(j in J) ( z >= x[j, sigma[j,m]] + p[j, sigma[j,m]])
;

output [ 
  "\nz: ", show(z), "\n",
  "x: "
] ++
[
  if a = 1 then "\n" else " " endif ++
    show(x[i,a])
  | i in J, a in M
] ++ [ "\nY:"] ++
[
  if a = 1 /\ j = 1 then "\n" else "" endif ++
  if a = 1 then "\n" else " " endif ++
    show(Y[i,j,a])
  | i,j in J, a in M
] ++ ["\n"]
;


%
% data
%

% """
% These data correspond to the instance ft06 (mt06) from:
%
%    H. Fisher, G.L. Thompson (1963), Probabilistic learning combinations
%    of local job-shop scheduling rules, J.F. Muth, G.L. Thompson (eds.),
%    Industrial Scheduling, Prentice Hall, Englewood Cliffs, New Jersey,
%    225-251 
% 
% The optimal solution is 55 
% """

n = 6;
m = 6;

sigma = array2d(J, M,  [
 3, 1, 2, 4, 6, 5, 
 2, 3, 5, 6, 1, 4, 
 3, 4, 6, 1, 2, 5, 
 2, 1, 3, 4, 5, 6, 
 3, 2, 5, 6, 1, 4, 
 2, 4, 6, 1, 5, 3]);

p = array2d(J, M,  [
 3, 6, 1, 7, 6, 3, 
10, 8, 5, 4, 10, 10, 
 9, 1, 5, 4, 7, 8, 
 5, 5, 5, 3, 8, 9, 
 3, 3, 9, 1, 5, 4, 
10, 3, 1, 3, 4, 9]);

