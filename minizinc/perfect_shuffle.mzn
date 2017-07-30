%
% Perfect shuffle (out-shuffle) in MiniZinc.
%
% Source
% http://mathworld.wolfram.com/Out-Shuffle.html
% """
% ...
% The numbers of out-shuffles needed to return a deck of n=2, 4, ... to its original 
% order are 1, 2, 4, 3, 6, 10, 12, 4, 8, 18, 6, 11, ... (Sloane's A002326), which is 
% simply the multiplicative order of 2 (mod n-1). For example, a deck of 52 cards 
% therefore is returned to its original state after eight out-shuffles, 
% since 2^8=1 (mod 51)
% """
%
% Number of shuffles required:
% http://www.research.att.com/~njas/sequences/A002326
%
%
% The following model implements perfect shuffle for _even_ number of elements, 
% i.e. 2*n. 
%
% n = 4, for 1..8 gives the following matrix:
% 1 2 3 4 5 6 7 8
% 1 5 2 6 3 7 4 8
% 1 3 5 7 2 4 6 8
% 1 2 3 4 5 6 7 8  <-- restored
% 1 5 2 6 3 7 4 8
% 1 3 5 7 2 4 6 8
% 1 2 3 4 5 6 7 8  <-- restored
% 1 5 2 6 3 7 4 8
%
% Model created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%


%
% note: for n >= 74 gives weird error messages, or 
% core dump. Maybe a too large matrix?)
% this is happening also for for some other n 
%
int: n = 26; % we will use 2*n below
int: t = 2*n; % t for twice

% From 
% http://www.research.att.com/~njas/sequences/table?a=2326&fmt=4
%
% TODO: Make a proper predicate for this...
% (
% """In other words, least m such that 2n+1 divides 2^m-1."""
% )
array[1..73] of 1..200: num_shuffles_arr =  
 [  
  1, 2,  4,  3, 6,  10,12, 4, 8, 18, %  1..
  6, 11, 20, 18,28,  5,10,12,36, 12, % 11..
  20,14, 12, 23,21,  8,52,20,18, 58, % 21..
  60, 6, 12, 66,22, 35, 9,20,30, 39, % 31..
  54,82,  8, 28,11, 12,10,36,48, 30, % 41..
 100,51, 12,106,36, 36,28,44,12, 24, % 51..
 110,20,100,  7,14,130,18,36,68,138, % 61..
  46,60, 28                          % 71..
]
;

int: num_shuffles = if n <= 73 then num_shuffles_arr[n] else t endif;

% note: we build the complete t x t matrix, although only num_shuffles is shown 
array[1..t, 1..t] of var 1..t: x;


%
% perfect shuffle (out shuffle) of an array 1..2*n
%
% For 2*n = 8 the permutation is
%
%    1 2 3 4 5 6 7 8
%    1 5 2 6 3 7 4 8
%    o e o e o e o e  (odd/even)
%
% This principle works for all sequences of 2*n.
% Note that here it is 1-based, but the implementation is 0-based:
% - The odd indices gets the positions 1..n, e.g. 1..4
% - The even indices gets the positions n+1..2*n, e.g. 5..8
%
% Note: The indices starts with 0 when calling with the array comprehension.
% This seems to be changed in later versions of MiniZinc spec. 
% This works at least in MiniZinc version 0.7.1 .
%
predicate perfect_shuffle(array[int] of var int: x, array[int] of var int: y, int: n) = 


    forall(i in 1..n) (
      trace("2*i: " ++ show(2*i-1) ++ " 2*i-1:" ++ show(2*i-1-1) ++ " n: " ++ show(n) ++
            " i: " ++ show(i-1) ++ " n+i:" ++ show(n+i-1) ++ "\n", 1=1)
      /\
      % place the odd indices
      y[2*i-1] = x[i]
      /\ % place the even indices
      y[2*i] = x[n+i]
   )


;

solve satisfy;
% solve :: int_search([x[i,j] | i,j in 1..t], "first_fail", "indomain_min", "complete") satisfy;

constraint
   % %% the first row is just 1..t
   % I changed it to the _last_ row, in order to end with the identity row.
   % (I'm amazed that it worked, since everything now must go "backwards".)
   forall(i in 1..t) (
     % x[1, i] = i
     x[num_shuffles, i] = i
   )
   /\
   forall(i in 2..t) (
     perfect_shuffle([x[i-1,j] | j in 1..t], [x[i,j] | j in 1..t ], n) 
   )
;

output [
   show(x[num_shuffles, j]) ++ " " 
   | j in 1..t
     
] ++ 
[
  if j = 1 then "\n" else " " endif ++
   show(x[i,j])
  | i in 1..num_shuffles, j in 1..t
] ++ 
[
  "\nnum_shuffles: " ++ show(num_shuffles)
] ++ ["\n"];

% output [
%   "x: ", show(x),"\n",
%   "y: ", show(y),"\n"
% ];
