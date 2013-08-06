%
% Photo problem in Minizinc.
%
% See photo_hkj2_data1.mzn .
%
% The data is from http://www.g12.cs.mu.oz.au/minizinc/photo.data2
% which is a slightly larger problem.
%
%
% Model created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%
 
% fz and ecl gives the following 8 solutions
% 12: [1, 3, 7, 8, 2, 0, 6, 5, 4, 9, 10]
% 12: [2, 0, 7, 8, 1, 3, 6, 5, 4, 9, 10]
% 12: [4, 0, 7, 8, 3, 2, 6, 5, 1, 9, 10]
% 12: [4, 2, 7, 8, 3, 0, 6, 5, 1, 9, 10]
% 12: [6, 8, 3, 2, 7, 10, 4, 5, 9, 1, 0]
% 12: [6, 10, 3, 2, 7, 8, 4, 5, 9, 1, 0]
% 12: [8, 10, 3, 2, 9, 7, 4, 5, 6, 1, 0]
% 12: [9, 7, 3, 2, 8, 10, 4, 5, 6, 1, 0]
%

include "photo_hkj2_model.mzn";

n = 11;
num_pref = 20;

% solve :: int_search(positions, "first_fail", "indomain", "complete") maximize z;
solve :: int_search(positions, first_fail, indomain_min, complete) satisfy;

% for solve satisfy
% constraint 
%   z >= 12
% ;

% The preference list from 
% http://www.g12.cs.mu.oz.au/minizinc/photo.data2
preferences =
array2d(1..num_pref, 1..2, [ 0,2, 0,4, 0,7, 
  1,4, 1,8, 
  2,3, 2,4, 
  3,0, 3,4, 3,9,
  4,5, 4,0, 
  5,0, 5,8, 
  6,2, 6,7, 
  7,8, 7,6, 
  8,9, 
  9,10
]);



output [
   show(z), ": ", show(positions), "\n",
];
