%
% Photo problem in Minizinc.
%
% Same as for photo_hkj.mzn with the following exeption:
%
%  - data is from 
%          http://www.g12.cs.mu.oz.au/minizinc/photo.data1
%  - uses the general model photo_hkj2_model.mzn
%  - the person index is from 0 .. n-1
% 
% This problem is also included in the gecode example collection photo.cc
% 
% Gecode solution:         
%  pos[] = {1, 3, 7, 8, 2, 0, 6, 5, 4}
%   sat: 10

% For the first problem fz and ecl both gives the following 20 solutions for z = 10
%
% 10: [1, 3, 7, 8, 2, 0, 6, 5, 4]    <- The gecode solution
% 10: [1, 8, 4, 3, 2, 0, 5, 6, 7]
% 10: [2, 0, 4, 3, 1, 8, 5, 6, 7]
% 10: [2, 0, 7, 8, 1, 3, 6, 5, 4]
% 10: [2, 8, 4, 3, 1, 0, 5, 6, 7]
% 10: [3, 0, 6, 5, 4, 2, 7, 8, 1]
% 10: [4, 0, 6, 5, 3, 2, 7, 8, 1]
% 10: [4, 0, 7, 8, 3, 2, 6, 5, 1]
% 10: [4, 2, 6, 5, 3, 0, 7, 8, 1]
% 10: [4, 2, 7, 8, 3, 0, 6, 5, 1]
% 10: [4, 6, 1, 0, 5, 8, 2, 3, 7]
% 10: [4, 6, 2, 3, 5, 8, 1, 0, 7]
% 10: [4, 8, 1, 0, 5, 6, 2, 3, 7]
% 10: [4, 8, 2, 3, 5, 6, 1, 0, 7]
% 10: [5, 8, 2, 3, 4, 6, 1, 0, 7]
% 10: [6, 0, 4, 5, 7, 8, 3, 2, 1]
% 10: [6, 8, 1, 0, 7, 5, 2, 3, 4]
% 10: [6, 8, 4, 5, 7, 0, 3, 2, 1]
% 10: [7, 0, 4, 5, 6, 8, 3, 2, 1]
% 10: [7, 5, 1, 0, 6, 8, 2, 3, 4]
%
% Model created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%


include "photo_hkj2_model.mzn";

n = 9;         % number of persons
num_pref = 17; % number of preferences 

% solve :: int_search(positions, first_fail, indomain_min, complete) maximize z;
solve :: int_search(positions, first_fail, indomain_min, complete) satisfy;

% for solve satisfy
% constraint
%  z >= 10
% ;

% The preference list from 
% http://www.g12.cs.mu.oz.au/minizinc/photo.data1
preferences =
array2d(1..num_pref, 1..2, 
[  0,2, 0,4, 0,7, 
   1,4, 1,8, 
   2,3, 2,4, 
   3,0, 3,4,
   4,5, 4,0, 
   5,0, 5,8, 
   6,2, 6,7, 
   7,8, 7,6
]);


output [
   show(z), ": ", show(positions), "\n"
];
