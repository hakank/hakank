%
% MiniZinc implementation of the example of Set Covering Deployment at
% 
% http://mathworld.wolfram.com/SetCoveringDeployment.html
%
% This Minizinc program was written by Hakan Kjellerstrand, hakank@bonetmail.com,
% and is commented in the (swedish) blog post
% Constraint Programming: Minizinc, Gecode/flatzinc och ECLiPSe/minizinc
% http://www.hakank.org/webblogg/archives/001209.html
%
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

int: n;                                           % number of countries
var int: c_armies = sum(i in 1..n) (X[i] + Y[i]); % number of armies (objective to minimize)

array[1..n, 1..n] of int: matrix;                 % the incidence matrix

array[1..n] of var 0..1: X;                       % the first army
array[1..n] of var 0..1: Y;                       % the second (reserve) army


solve minimize c_armies;
% use this when constraint 3 is activated: show all optimal solutions
% solve satisfy; 

%
% Constraints
%

% Constraint 1: There is always an army in a city (+ maybe a backup)
%               Or rather: Is there a backup, there must be an an army
constraint 
           forall(i in 1..n) (
              X[i] >= Y[i]
           )
;

% Constraint 2: There should always be an backup army near every city
constraint 
           forall(i in 1..n) (
                     (X[i] + sum(j in 1..n where matrix[i,j] = 1) (Y[j]))  >= 1
           )
;

% Constraint 3 for showing all the solutions. 
% After a 
%   solve minimize 
% we know that the required number of armies is 4. 
% Now we could use 
%    solve satisfy 
% to see every solution.
% constraint 
%            sum(i in 1..n) (X[i] + Y[i]) <= 4
% ;


% data
n = 8;

matrix = 
array2d(1..n, 1..n,
[ 0,   1,   0,   1,   0,   0,   1,   1,
  1,   0,   0,   1,   0,   0,   0,   0,
  0,   0,   0,   0,   1,   1,   0,   0,
  1,   1,   0,   0,   0,   0,   1,   0,
  0,   0,   1,   0,   0,   1,   1,   0,
  0,   0,   1,   0,   1,   0,   1,   1,
  1,   0,   0,   1,   1,   1,   0,   1,
  1,   0,   0,   0,   0,   1,   1,   0
])
;


output [
       "[alexandria, asia_minor, britain, byzantium, gaul, iberia, rome, tunis]","\n",
       "X: ", show(X),"\n",
       "Y: ", show(Y),"\n",
       "c_armies: ", show(c_armies), "\n"
]
