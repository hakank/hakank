% 
% "Calvin puzzle" in MiniZinc.
%
% From "An Exercise for the Mind: A 10 by 10 Math Puzzle: 
%       A Pattern Recognition Game: Meditation on an Open Maze" 
% http://www.chycho.com/?q=Puzzle
% """
% The Purpose of the Game
%
% To take a 10 by 10 grid, representing 100 squares, and completely fill every square 
% based on two types of movements. 
% ...
%
% Movement Type I)  If the next number in the sequence is going to be placed vertically 
% or horizontally, then it must be placed exactly three squares away from the previous 
% number (there must be a two square gap between the numbers).
%
% Movement Type II) If the next number in the sequence is going to be placed diagonally, 
% then it must be placed exactly two squares away from the previous number (there must 
% be a one square gap between the numbers). 
% """

% Solution for n = 5 (0.5 second, 885 failures)
%
%   1  8  5  2 11
%  16 21 24 15 18
%   6  3 12  7  4
%  23  9 17 22 10
%  13 20 25 14 19
%

% n = 6 (15 seconds, 112363 failures)
%    1  8  5  2  9  6
%   16 13 23 26 14 33
%   30  3 10  7  4 19
%   22 25 15 34 24 27
%   17 12 31 18 11 32
%   29 35 21 28 36 20

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

include "globals.mzn"; 

int: n = 5; %  5 is fast

% solutions
array[1..n, 1..n] of var 1..n*n: x;

% solve satisfy;
solve :: int_search(
   [x[i,j] | i in 1..n, j in 1..n], 
   smallest, % occurrence, % smallest, anti_first_fail, 
   indomain_min, % indomain_median, 
   complete) 
   satisfy;

constraint

   % place all integers from 1..n*n
  all_different([x[i,j] | i in 1..n, j in 1..n]) % :: domain

  % /\ % symmetry breaking
  % x[1,1] = 1 % faster without this

  /\ % This variant of "expanding" the exists are more efficient
  forall(k in 1..n*n-1) (
     let {
        var 1..n: i,
        var 1..n: j,
        var {-3,-2,0,2,3}: a,
        var {-3,-2,0,2,3}: b
     }
     in
     k = x[i, j] % fix this k
     /\ 
     % find the next k
     k + 1 = x[i+a, j+b]  
     /\
     i+a >= 1 /\ j+b >=  1 /\
     i+a <= n /\ j+b <= n
     /\
     (  % The legal moves
        (abs(a) = 2 /\ abs(b) = 2)
        \/
        (abs(a) = 3 /\ b = 0)
        \/
        (abs(b) = 3 /\ a = 0)
     )
  ) 

;

output 
% [
%  "pos: ", show(pos),
% ] ++
[
  if j = 1 then "\n" else " " endif ++
    show_int(2, x[i,j])
  | i in 1..n, j in 1..n

];
