%
% Map coloring (Australia) in Minizinc
% 
% Example från Marriot & Stuckey "Programming with Constraints", 
% page 86.
%
% 
% Model created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%
include "globals.mzn";

int: n;                                % number of countries
array[1..n] of var 1..n: country;      % the countries 
array[1..n,1..n] of 0..1: connections; % the map
var int: num_larger;                   % symmetry breaking

% 
% color the countries
%
predicate coloring(array[int,int] of int: conn, array[int] of var int: countries, int: num_countries) =
           forall(i,j in 1..num_countries where i != j /\ conn[i,j] = 1) (
              countries[i] != countries[j]
           )
           /\ % symmetry breaking
           num_larger = sum(i,j in 1..num_countries where i != j) (
              bool2int(countries[i] > countries[j])
           )
;

constraint  
   coloring(connections, country, n) /\
   country[1] = 1
;

solve satisfy;
% alternative objectives
% solve minimize num_larger;
% solve minimize sum(i in 1..n) (i*country[i]);
% solve maximize sum(i in 1..n) (i*country[i]);

% data
n = 7; % number of countries


% Note: T does not border to any country
connections = 
% T  WA NT SA Q NSW V
array2d(1..n, 1..n,
[
  0, 0, 0, 0, 0, 0, 0, % T (has no connection)
  0, 1, 1, 1, 0, 0, 0, % WA
  0, 1, 1, 0, 1, 0, 0, % NT
  0, 1, 1, 1, 1, 1, 0, % SA
  0, 0, 1, 1, 1, 1, 0, % Q
  0, 0, 0, 1, 1, 1, 1, % NSW
  0, 0, 0, 1, 0, 1, 0  % V

]);

output [
   " T  WA NT SA Q NSW V:\n",
   show(country), "\n",
]
;

