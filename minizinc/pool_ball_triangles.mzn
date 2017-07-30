% 
% Pool-ball triangles problem in MiniZinc.
% 
% Martin Gardner:
% Given n*(n+1) div 2 numbered pool balls in a triangle, is it 
% possible to place them so that the number of each ball below 
% two balls is the difference of (the number of) those two balls?
% (In the problem, n=5, i.e. the numbers 1..15)
% 
% Index of balls for n=5:
% 16 17 18 19 20 21
%  11 12 13 14 15 
%    7  8  9  10
%     4   5  6
%       2  3
%         1

%
% Note:
% - This model don't handle mirror symmetries of the triangle.
%   Later addendum (see below): Yes, it does.
% - There are no solutions for n=6,7
%  

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

include "globals.mzn"; 

% int: n = 4; % n'th triangle number 1,3,6,10,15,...
% int: n = 5; 
int: n = 3; 
int: len = (n*(n + 1)) div 2;
array[1..len] of var 1..len: x;

% the triangle numbers for 1..n
array[1..n] of 1..len: t = [i*(i+1) div 2 | i in 1..n] ; 

% the index of first number to use in the subtraction
array[1..t[n-1]] of var 1..len: subs; 


predicate contains(var int: e, array[int] of var int: a) =
   exists(i in 1..length(a)) (
      a[i] = e
   )
;


% solve satisfy;
solve :: int_search(x ++ subs, first_fail, indomain_split, complete) satisfy;

constraint

   % create the array of numbers to subtract
   subs[1] = 2
   /\ 
   forall(i in 2..t[n-1]) (
      % "jump" of two when i-1 is a triangle number
      ( contains(i-1,t) ->  subs[i] = subs[i-1] + 2 )
      /\
      ( not (contains(i-1, t)) ->  subs[i] = subs[i-1] + 1  )
   )  
 
   /\ % position the balls in their places
   forall(i in 1..t[n-1]) (
      x[i] = abs(x[subs[i]]-x[subs[i]+1])
   )
   /\
   all_different(x)

   /\ % symmetry breaking
   x[2] < x[3] 
;

output [
 "x: ", show(x), "\n",
% "t: ", show(t), "\n",
% "subs: ", show(subs), "\n",
];