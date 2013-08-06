% 
% Cookie Monster Problem in MiniZinc.
% 
% Richard Green
% "The Cookie Monster Problem"
% https://plus.google.com/u/0/101584889282878921052/posts/8qWvSaLJVGD
% """
% Suppose that we have a number of cookie jars, each containing a certain number of cookies. 
% The Cookie Monster wants to eat all the cookies, but he is required to do so in a number 
% of sequential moves. At each move, the Cookie Monster (CM) chooses a subset of the jars, 
% and eats the same (nonzero) number of cookies from each jar. The goal of the CM is to 
% empty all the cookies from the jars in the smallest possible number of moves, and the 
% Cookie Monster Problem is to determine this number for any given set of cookie jars.
% 
% Since the CM has an unlimited appetite, there is essentially no difference between eating 
% three cookies from one jar, and eating three cookies from each of 10 jars. It turns out 
% that there is no advantage to depleting these 10 jars at different rates, so the 10 jars 
% may as well be one jar, and we may as well reduce to the case where no two jars contain 
% the same number of cookies. It is also safe to ignore any empty jars. This means that the 
% starting state of the problem may be described by a set of positive integers, S. 
% The Cookie Monster Number of S, CM(S), is the smallest number of moves in which this set 
% of jars can be completely emptied.
%
% Let's look at an example. Suppose that S is the set {15, 13, 12, 4, 2, 1}, meaning that there 
% are six jars, containing 1, 2, 4, 12, 13 and 15 cookies each. Here are three possible 
% strategies open to the CM.
%
% ...
% 
% The recent paper http://arxiv.org/abs/1304.7508 by Megan Belzner gives a good introduction 
% to the Cookie Monster Problem. The paper looks at some special cases of the problem, 
% including the case where the set S has size 3. In this case, it takes three moves to 
% empty S, unless the sum of the cookies in two of the jars equals the number in the 
% third, in which case it only takes two moves. Some other cases examined in the paper 
% are arithmetic progressions (which sometimes achieve the lower bound), geometric 
% progressions (which sometimes achieve the upper bound) and the Fibonacci sequence 
% (which can be emptied in a number of moves roughly equal to half the number of jars). 
% The problem can also be altered to form a two-player game, similar to the game of Nim, 
% and the author discusses this to some extent.
% """

%
% For this problem instance there are 28 different solutions with 4 moves (optimal solution)
%   z: [12, 1, 2, 1]
%   z: [11, 1, 3, 1]
%   z: [11, 1, 2, 1]
%   z: [11, 1, 1, 2]
%   z: [4, 8, 2, 1]
%   z: [11, 4, 1, 1]
%   z: [12, 4, 2, 1]
%   z: [11, 4, 2, 1]
%   z: [10, 4, 2, 1]
%   z: [9, 4, 2, 1]
%   z: [8, 4, 2, 1]
%   z: [11, 2, 1, 1]
%   z: [10, 3, 1, 1]
%   z: [12, 2, 1, 1]
%   z: [12, 3, 1, 1]
%   z: [1, 11, 1, 2]
%   z: [11, 3, 1, 1]
%   z: [12, 3, 2, 1]
%   z: [12, 2, 2, 1]
%   z: [11, 3, 2, 1]
%   z: [11, 2, 2, 1]
%   z: [10, 2, 2, 1]
%   z: [10, 3, 2, 1]
%   z: [9, 3, 2, 1]
%   z: [3, 9, 2, 1]
%   z: [1, 11, 2, 1]
%   z: [2, 10, 2, 1]
%   z: [1, 11, 3, 1]
%

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@gmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc/
%

include "globals.mzn"; 

int: n = 6;
array[1..n] of int: jars = [15, 13, 12, 4, 2, 1];
int: max_moves = 5; % including 1 for the initial entry in x

% decision variables

% cookies left are eating z cookies from each jar
array[1..max_moves, 1..n] of var 0..max(jars): x;
array[1..max_moves] of var 0..max(jars): z;
var 1..max_moves: num_moves; % number of moves needed


solve :: int_search(
        z ++ [x[m,i] | m in 1..max_moves, i in 1..n], 
        max_regret, % first_fail, 
        indomain_split, 
        complete) 
    minimize num_moves;
% solve minimize num_moves;

constraint
 forall(i in 1..n) (
    x[1,i] = jars[i]
 )
 /\
 z[1] = 0
;

constraint
 forall(m in 2..max_moves) (   
    forall(i in 1..n) (
       (
       (z[m] <= x[m-1,i] -> x[m,i] = x[m-1,i] - z[m] )
       /\
       (z[m] > x[m-1,i] -> x[m,i] = x[m-1,i])
       )
    )
  )
;

constraint
  exists(m in 2..max_moves) (
    num_moves = m-1 /\ 
    forall(i in 1..n) ( x[m,i] = 0)
    /\
    forall(m2 in m+1..max_moves) ( z[m2] = 0)
  )

  % /\ num_moves = 4 % for solve satisfy
  % /\ z[2] = 8 % Testing
;

% Experimental: Symmetry breaking
% constraint decreasing([z[m] | m in 2..max_moves]);

output [
  "Num_moves: " ++ show(num_moves) ++ "\n" ++
  "Cookies picked: " ++ show([z[m] | m in 2..fix(num_moves)+1]) ++ "\n"
]
++
[
  "jars:             " ++ show(jars)
]
++
[
  if i = 1 then "\nTake " ++ show_int(2, z[m]) ++ " cookie(s): "  else " " endif ++ 
    show(x[m,i])
  | m in 2..max_moves, i in 1..n
]
;

