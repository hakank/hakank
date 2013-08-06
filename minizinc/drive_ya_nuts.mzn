% 
% Drive Ya Nuts puzzle in MiniZinc.
% 
% From http://www.samstoybox.com/toys/DriveYaNuts.html
% """
% The Drive Ya Nuts puzzle by Milton Bradley was cool and quite difficult. The object of 
% the puzzle is to place all seven nuts such that the numbers on all sides of each 
% nut match the numbers on the adjoining nut. There is but one way to solve the puzzle. 
% Here are two versions of puzzle. Note that the second one is still factory sealed and 
% shows the solution. So you think it sounds easy? 
% """
%
% Some other links: 
% - http://www.jaapsch.net/puzzles/circus.htm
%
% Representation:
%
% A side of a nut is numbered as following
% 
%            1
%
%       6        2
%   
%       5        3
%
%            4
%
%
% and the 7 nuts are numbered as follows:
%
%            1 
%
%        6       2
%            7
%        5        3
% 
%            4
% 
% i.e. nut 7 is the master (center) nut.
%
%
% Note: There are 6 solutions, depending on how we orient
%       the center nut (7). This is handled by symmetry breaking below.
%
% Here is one solution (which has the center nut start with 1):
% 
%    2 3 5 1 4 6  % Nut 1 (in the representation above)
%    3 2 4 1 6 5  % Nut 2
%    1 4 3 6 5 2  % Nut 3
%    4 5 6 1 2 3  % Nut 4
%    2 5 3 1 6 4  % Nut 5
%    5 4 3 2 1 6  % Nut 6
%    1 6 2 4 5 3  % Nut 7 (center nut)
%
% E.g. the first nut is the nut 1,4,6,2,3,5 rotated like this, i.e.
% with 2 at 12 o'clock and then clock wise: 2,3,5,1,4, and 6:
%    
%            2
%
%       6        3
%   
%       4        5
%
%            1
%
% And so on with the other nuts.
% 
%
% Note: I started with this MiniZinc model after reading the Frink 
% implementation by Alan Eliasen 
%     http://futureboy.us/fsp/colorize.fsp?f=DriveYaNuts.frink
% which had the link cited above. The Frink program use a different 
% approach, though.
%
% 
% [Personal comment: 
%  This is the same puzzle as the infamous AWA-Patent problem 
% from a long long time ago, though I didn't then know what 
% it was called.
% Yes, I did solve it manually without any computational help.]
%
% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@gmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc/
%

include "globals.mzn"; 

int: m = 7;
int: n = 6;
array[1..m, 1..n*2] of var 1..n: nuts;
int: num_connections;
array[1..num_connections, 1..4] of int: connections;

% decision variables
array[1..m,1..n] of var 1..n: x; % the final solution

array[1..m] of var 1..m: pos; % which nut is this? (in the nuts matrix)
array[1..m] of var 1..m: pos_inv; % Permutation array of pos.
% indices to start the nut (in the nuts m x n*2 matrix)
array[1..m] of var 0..n-1: start_ix;

% solve satisfy;
solve :: int_search(
        pos ++ [x[i,j] | i in 1..m, j in 1..n], 
        first_fail, 
        indomain_min, 
        complete) 
    satisfy;

constraint
   forall(i in 1..m) (
      alldifferent([x[i,j] | j in 1..n]) /\

      % for some "rotation" of each nut...
      let {
         var 0..n-1: k, % the "rotation" (i.e. shift in the n*2 array)
         var 1..m: p    % which nut is at position pos[i] 
      } in
         start_ix[i] = k
         /\
         pos[i] = p
         /\
         forall(j in 1..n) (x[i,j] = nuts[p,j+k]) % offset by k
   ) 
   /\
   alldifferent(pos) 

   /\ % for display (and pondering)
   inverse(pos, pos_inv)

   % symmetry breaking: 
   % We pick the solution where the center nut (nut 7) start with 1.
   /\ start_ix[7] = 0

;

% check the connections
constraint
   forall(c in 1..num_connections) (
        let {
          array[int] of int: cc = [connections[c,j] | j in 1..4]
      } in
      x[cc[1], cc[3]] = x[cc[2], cc[4]]
   )
;


%
% Checking on nuts. 
%
% This is not needed for the standard problem but can be used to generate 
% problem instances.
%
constraint
   forall(i in 1..m) (
      nuts[i,1] = 1
      /\
      forall(j in 1..n) (
         nuts[i,j] = nuts[i,j+n]
      )
   )

   % % Symmtery breaking: ordered rows.
   % % Note: If using the static problem instances, this should be commented.
   % /\
   % forall(i in 2..m) (
   %   lex_less([nuts[i-1,j] | j in 1..n], [nuts[i,j] | j in 1..n] )
   % )
;


output 
[
  "pos     : " ++ show(pos) ++ "\n" ++
  "pos_inv : " ++ show(pos_inv) ++ "\n" ++
  "start_ix: " ++ show(start_ix) ++ "\n" ++
  "x:"
]
++
[
   if j = 1 then "\n" else " " endif ++
     show(x[i,j]) ++ 
     if i = 7 /\ j = n then "  center!" else ""  endif
   | i in 1..m, j in 1..n
] 
++
[
 "\ncenter (" ++ show(7) ++ "): " ++ show([nuts[pos[7],j] | j in 1..n]) ++ "\n" 
]
++
[ 
  "\nnuts:"
]
++
[
  if j = 1 then "\n" else " " endif ++
    show(nuts[i,j])
  | i in 1..m, j in 1..n
]
++
["\n"]
;

%
% The connection points between the nuts, i.e. where the values
% must be the same.
% (Not surprisingly there are some permutations involved.)
%
num_connections = 12;
connections = array2d(1..num_connections, 1..4,
   [
     % nuts    sides to be equal
     1,2,       3,6,
     2,3,       4,1,
     3,4,       5,2,
     4,5,       6,3,
     5,6,       1,4,
     6,1,       2,5,

     7,1,       1,4,
     7,2,       2,5,
     7,3,       3,6,
     7,4,       4,1,
     7,5,       5,2,
     7,6,       6,3,
   ]);
     

%
% Problem instances.
%
% Note: If using these, it might be a good idea to comment out the 
%       lex_less symmetry breaking in the "Checking on nuts" above
%       (unless the "sorted" instance below is used).
%


% This is the nuts in the solution order.
% nuts = array2d(1..m, 1..n*2,
%   [
%     1,4,6,2,3,5, 1,4,6,2,3,5, % 1 
%     1,6,5,3,2,4, 1,6,5,3,2,4, % 2 
%     1,4,3,6,5,2, 1,4,3,6,5,2, % 3
%     1,2,3,4,5,6, 1,2,3,4,5,6, % 4
%     1,6,4,2,5,3, 1,6,4,2,5,3, % 5
%     1,6,5,4,3,2, 1,6,5,4,3,2, % 6 
%     1,6,2,4,5,3, 1,6,2,4,5,3, % 7 % center nut
%   ]);


% "arbitrary" order (sorted)
% Note that pos_inv for the shown solution is the permutation 
% [4,3,1,7,5,2,6].
nuts = array2d(1..m, 1..n*2,
  [
    1,2,3,4,5,6, 1,2,3,4,5,6, % 4 (row 4 in the solution order shown above)
    1,4,3,6,5,2, 1,4,3,6,5,2, % 3
    1,4,6,2,3,5, 1,4,6,2,3,5, % 1 
    1,6,2,4,5,3, 1,6,2,4,5,3, % 7 [center nut]
    1,6,4,2,5,3, 1,6,4,2,5,3, % 5
    1,6,5,3,2,4, 1,6,5,3,2,4, % 2 
    1,6,5,4,3,2, 1,6,5,4,3,2, % 6 
  ]);


% Another order
% nuts = array2d(1..m, 1..n*2,
%   [
%     1,6,5,3,2,4, 1,6,5,3,2,4, % 2 
%     1,2,3,4,5,6, 1,2,3,4,5,6, % 4 (row 4 in the solution order shown above)
%     1,6,2,4,5,3, 1,6,2,4,5,3, % 7 [center nut]
%     1,6,4,2,5,3, 1,6,4,2,5,3, % 5
%     1,6,5,4,3,2, 1,6,5,4,3,2, % 6 
%     1,4,6,2,3,5, 1,4,6,2,3,5, % 1 
%     1,4,3,6,5,2, 1,4,3,6,5,2, % 3
%   ]);
