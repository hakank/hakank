%
% Set covering in MiniZinc.
%
% Example 9.1-2, page 354ff, from Taha "Operations Research - An Introduction"
% Minimize the number of security telephones in street corners on a campus.
%
% AMPL model: http://taha.ineg.uark.edu/setcover.txt
%

% 
% MiniZinc solutions with solve satisfy:
% z = 4
% x = [0, 1, 0, 0, 1, 1, 1, 0]
% 
% z = 4
% x = [1, 1, 0, 0, 1, 0, 1, 0]
%

int: n;   % maximum number of corners
int: num_streets; % number of connected streets
set of int: streets = 1..num_streets;
array[streets] of set of streets: corner;

array[1..n] of var 0..1: x;
var int: z= sum(j in 1..n) (x[j]);

solve :: int_search(x, first_fail, indomain_min, complete) minimize z;
% solve satisfy;

constraint
 % z <= 4 % for solve satisfy
 % /\
 forall(i in streets) (
     sum(j in corner[i]) (x[j])>=1
 )
;

%
% data
% 
n = 8;
num_streets = 11;

% corners of each street
corner = [
  {1,2},
  {2,3},
  {4,5},
  {7,8},
  {6,7},
  {2,6},
  {1,6},
  {4,7},
  {2,4},
  {5,8},
  {3,5}
];


output [show(x)];